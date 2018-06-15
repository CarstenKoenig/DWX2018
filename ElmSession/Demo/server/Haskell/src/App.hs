{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module App
    ( startApp
    , app
    ) where


import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified RestApi
import           RestApi (RestApi)
import qualified RouteApi
import           RouteApi (RouteApi)


getPort :: IO Int
getPort = return 8080


getDbPath :: IO FilePath
getDbPath = return "./todos.db"


startApp :: IO ()
startApp = do
  dbPath <- getDbPath
  putStrLn $ "initializing Database in " ++ dbPath
  handle <- Db.initDb dbPath

  port <- getPort
  putStrLn $ "starting Server on " ++ show port
  run port $ app handle


app :: Db.Handle -> Application
app handle = 
  Servant.serve (Proxy :: Proxy (RestApi :<|> RouteApi)) $
    RestApi.server handle :<|> RouteApi.server