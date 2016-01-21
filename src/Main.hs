module Main where

import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)
import Database.Persist.Sqlite     (runSqlPool)

import Config (defaultConfig, Config(..), Environment(..), setLogger, makePool)
import Waku.ExternalAccess.Server   (app)
import Waku.ExternalAccess.Database (doMigrations)
import Waku.APIs.ExternalAccessAPI  (externalAccessAPI)
import Servant.Docs

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8084
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

apiDocs :: API
apiDocs = docs externalAccessAPI
updateDocs = writeFile "external-access-service.md" $ markdown apiDocs
