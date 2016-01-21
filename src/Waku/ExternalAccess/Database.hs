{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Waku.ExternalAccess.Database where

import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist.Sqlite     (SqlBackend(..),runMigration
                                   ,runSqlPool,fromSqlKey,Entity(..))
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Data.Time
import Data.Text
import Data.Int                    (Int64,Int)
import qualified Data.UUID as U

import Config
import Waku.Models.General
import qualified Waku.Models.ExternalAccess as M
import Waku.Models.ExternalAccess (AccessLevel(..))
import Waku.ExternalAccess.AccessLevel

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ExternalAccess
    uuid String
    userId Id
    creatorId Id
    email String
    alias String Maybe
    created UTCTime
    expires UTCTime Maybe
    accessRevoked Bool 
    level AccessLevel
    contentId ContentId
    contentType ContentType
    UniqueUuid uuid
    UniqueExternalAccess email contentId contentType
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

entityToModel :: ExternalAccess -> M.ExternalAccess
entityToModel (ExternalAccess {..}) = M.defaultExternalAccess
    { M.externalaccessUuid = maybe U.nil id $ U.fromString externalAccessUuid
    , M.externalaccessCreatorId = externalAccessCreatorId
    , M.externalaccessEmail = externalAccessEmail
    , M.externalaccessAlias = externalAccessAlias
    , M.externalaccessCreated = externalAccessCreated
    , M.externalaccessExpires = externalAccessExpires
    , M.externalaccessAccessRevoked = externalAccessAccessRevoked
    , M.externalaccessAccessibleContent = ContentKey externalAccessContentId externalAccessContentType
    , M.externalaccessLevel = externalAccessLevel
    }
