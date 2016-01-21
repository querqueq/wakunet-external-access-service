{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Waku.ExternalAccess.Server where

import Data.Maybe                   (fromJust)
import Data.Either.Combinators      (mapBoth)
import Control.Monad    
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, runReaderT, lift, MonadReader
                                    , liftIO)
import Control.Monad.Trans.Either   (EitherT, left, right, runEitherT, bimapEitherT)
import Network.Wai                  (Application)
import Data.Time                    (UTCTime(..),getCurrentTime)
import Data.Text                    (Text, pack, unpack, split)
import Text.Read                    (readMaybe)
import Data.Time.Format
import Data.Int                     (Int64)
import Database.Persist
import Database.Persist.Types       (PersistValue(..))
import Database.Persist.Sqlite
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import qualified Data.Map as M

import Servant hiding (contentType)
import Servant.Docs

import Config
import Waku.Models
import Waku.Models.General
import Waku.APIs.ExternalAccessAPI
import Waku.Clients.DiscussionClient
import Waku.Clients.UserClient 
import Waku.Clients.MailClient
import Waku.Clients.ProfileClient
import Waku.Clients.ChatClient
import Waku.ExternalAccess.Database (runDb)
import Waku.Servers.Util
import Waku.Servers.Errors
import qualified Waku.ExternalAccess.Database as DB

import Debug.Trace

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT ExternalAccessAPI AppM
server = createExternalAccess -- !
    :<|> updateExternalAccess --
    :<|> getExternalAccess --
    :<|> getAccessibleContent --
    :<|> postAccessibleContent --
    :<|> setAlias --
    :<|> revokeExternalAccess --
    :<|> getExternalAccessesForContent --
    :<|> trace "before call" . notifyExternalUser --

getContent :: Id -> ContentId -> ContentType -> EitherT ServantErr IO Content
getContent senderId cid "post" = bimapEitherT errForward ContentDiscussion $ getDiscussion (Just senderId) cid
getContent senderId cid "chat" = bimapEitherT errForward ContentChatDescriptor $ getChatDescriptor (Just senderId) cid
getContent _ _ _ = left $ err404

createContent :: Id -> Content -> EitherT ServantErr IO Content
createContent senderId (ContentDiscussion disc) = bimapEitherT errForward ContentDiscussion 
    $ createDiscussion (Just senderId) disc
createContent _ _ = left $ err406 {errReasonPhrase = "content type not supported"}

selectExternalAccess (ExternalAccessRequest {..})= selectFirst 
    [DB.ExternalAccessEmail ==. externalaccessrequestEmail
    ,DB.ExternalAccessContentId ==. contentId ckey
    ,DB.ExternalAccessContentType ==. contentType ckey
    ] []
    where ckey = externalaccessrequestAccessibleContent

-- | Create external access - TODO refactor this!
createExternalAccess :: Maybe Id -> ExternalAccessRequest -> AppM AString
createExternalAccess Nothing _ = forbidden
createExternalAccess juid@(Just uid) ear@(ExternalAccessRequest {..}) = do
    duplicateEa <- runDb $ selectExternalAccess ear
    case duplicateEa of
        (Just (Entity _ v)) -> return $ AString $ DB.externalAccessUuid v
        Nothing -> do
            -- Check if an external access with email exists thus an user exists
            existingEa <- runDb $ selectFirst [DB.ExternalAccessEmail ==. externalaccessrequestEmail] []
            userId <- do
                x <- liftIO $ createUserId existingEa
                return $ mapBoth errForward id x
            case userId of
                (Left x) -> lift $ left x
                (Right id) -> do
                    now <- liftIO getCurrentTime
                    uuid <- liftIO U.nextRandom
                    (Entity _ newEa) <- runDb $ insertEntity $ DB.ExternalAccess
                        { DB.externalAccessUuid = U.toString uuid
                        , DB.externalAccessUserId = id
                        , DB.externalAccessCreatorId = uid
                        , DB.externalAccessEmail = externalaccessrequestEmail
                        , DB.externalAccessAlias = Nothing
                        , DB.externalAccessCreated = now
                        , DB.externalAccessExpires = externalaccessrequestExpires
                        , DB.externalAccessAccessRevoked = False
                        , DB.externalAccessContentId = contentId externalaccessrequestAccessibleContent
                        , DB.externalAccessContentType = contentType externalaccessrequestAccessibleContent
                        , DB.externalAccessLevel = externalaccessrequestLevel
                        }
                    return $ AString $ U.toString uuid 
    where 
        -- reuse user
        createUserId (Just (Entity _ x)) = return $ Right $ DB.externalAccessUserId x
        -- create new user
        createUserId Nothing = (liftM $ fmap userId) $ runEitherT $ registerUser juid $ profile uid (Just externalaccessrequestEmail)
        -- FIXME use profile generated from ea
        profile userId email = defaultProfile 
            { profileUserId = userId
            , profileEmail = email
            , profileFirstName = ""
            , profileSurname = ""
            }

filterUuid uuid = DB.ExternalAccessUuid ==. U.toString uuid

updateExternalAccess :: Maybe Id -> U.UUID -> ExternalAccessRequest -> AppM ExternalAccess
updateExternalAccess Nothing _ _ = forbidden
updateExternalAccess _ uuid ear@(ExternalAccessRequest {..}) = do
    existingEa <- runDb $ selectFirst [filterUuid uuid] []
    case existingEa of
        Nothing -> lift $ left err404
        Just (Entity k _) -> do
            runDb $ update k
                [DB.ExternalAccessEmail =. externalaccessrequestEmail
                ,DB.ExternalAccessExpires =. externalaccessrequestExpires
                ,DB.ExternalAccessLevel =. externalaccessrequestLevel
                ,DB.ExternalAccessContentId =. contentId ckey
                ,DB.ExternalAccessContentType =. contentType ckey
                ]
            ea <- runDb $ get k
            return $ DB.entityToModel $ fromJust ea
    where ckey = externalaccessrequestAccessibleContent

selectMaybe filters f = do
    e <- runDb $ selectFirst filters []
    case e of
        Nothing  -> lift $ left err404
        (Just (Entity _ v)) -> return $ f v

getExternalAccess :: U.UUID -> AppM ExternalAccess
getExternalAccess uuid = selectMaybe [filterUuid uuid] DB.entityToModel

getAccessibleContent :: U.UUID -> AppM ExternalContent
getAccessibleContent uuid = do
    now <- liftIO $ getCurrentTime
    ea@(DB.ExternalAccess {..}) <- selectMaybe [filterUuid uuid] id
    lift $ require 
            [(maybe False (<now) externalAccessExpires, errExpired)
            ,(externalAccessAccessRevoked, errRevoked)
            ] 
         $ do
            content <- contentForEa ea
            profiles <- bimapEitherT errForward id $ getPartialProfiles $ ProfileRequest $ participatingUsers content 
            return $ ExternalContent content (DB.entityToModel ea) profiles
    where contentForEa (DB.ExternalAccess {..}) = getContent externalAccessCreatorId externalAccessContentId externalAccessContentType

postAccessibleContent :: U.UUID -> Content -> AppM ()
postAccessibleContent uuid c = do
    now <- liftIO $ getCurrentTime
    ea@(DB.ExternalAccess {..}) <- selectMaybe [filterUuid uuid] id
    lift $ require 
            [(maybe False (<now) externalAccessExpires, errExpired)
            ,(externalAccessAccessRevoked, errRevoked)
            ,(externalAccessLevel >= Write, newErr 403 "write privileges required")
            ]
         $ (createContent (DB.externalAccessUserId ea) c) >> return ()

require :: [(Bool,ServantErr)] -> EitherT ServantErr IO a -> EitherT ServantErr IO a
require checks f = case foldl (\errs (x,err) -> if x then err:errs else errs) [] checks of
    [] -> f
    x:_ -> left x

-- | FIXME check if an entity was updated. if none 404
updateOne :: U.UUID -> [Update DB.ExternalAccess] -> AppM ()
updateOne uuid updates = runDb $ updateWhere [filterUuid uuid] updates

setAlias :: U.UUID -> AString -> AppM ()
setAlias uuid (AString alias) = updateOne uuid [DB.ExternalAccessAlias =. Just alias]

revokeExternalAccess :: Maybe Id -> U.UUID -> AppM ()
revokeExternalAccess Nothing _ = forbidden
revokeExternalAccess (Just sender) uuid = updateOne uuid [DB.ExternalAccessAccessRevoked =. False]
                        -- ^ FIXME check if sender is creator

getExternalAccessesForContent :: Maybe Id -> ContentType -> ContentId -> AppM [ExternalAccess]
getExternalAccessesForContent Nothing _ _ = forbidden
getExternalAccessesForContent (Just sender) ct cid = runDb $ selectList 
    [DB.ExternalAccessContentId ==. cid
    ,DB.ExternalAccessContentType ==. ct
    ] [] >>= return . (map (\(Entity _ v) -> DB.entityToModel v))

notifyExternalUser :: Maybe Id -> U.UUID -> Url -> AppM ()
notifyExternalUser Nothing _ _ = forbidden
notifyExternalUser (Just sender) uuid (Url url) = do
    (DB.ExternalAccess {..}) <- selectMaybe [filterUuid uuid] id
    lift $ bimapEitherT errForward id $ trace "before send mail" $ sendMail 
         $ Mail [externalAccessEmail] "WakuNet - Zugriff auf" "external-access-de" 
         $ M.fromList [("url",pack url)
                      ,("thing",thing externalAccessContentType)
                      ]
    return $ trace "end" ()

thing "post" = "einer Diskussion"
thing _ = "" -- FIXME what if content type is unknown?

forbidden :: ReaderT Config (EitherT ServantErr IO) a
forbidden = lift $ left err403

errExpired = newErr 403 "external access expired"
errRevoked = newErr 403 "external access was revoked"

app :: Config -> Application
app cfg = serve externalAccessAPI (readerServer cfg)

readerServer :: Config -> Server ExternalAccessAPI
readerServer cfg = enter (readerToEither cfg) server
 
readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg
