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
import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified RestApi
import           RestApi (RestApi)
import qualified RouteApi
import           RouteApi (RouteApi)
import qualified Ws


getPort :: IO Int
getPort = return 8080


getDbPath :: IO FilePath
getDbPath = return "./todos.db"


startApp :: IO ()
startApp = do
  dbPath <- getDbPath
  putStrLn $ "initializing Database in " ++ dbPath
  dbHandle <- Db.initDb dbPath
  wsHandle <- Ws.initialize

  port <- getPort
  putStrLn $ "starting Server on " ++ show port
  run port $ app dbHandle wsHandle


app :: Db.Handle -> Ws.Handle -> Application
app dbHandle wsHandle =
  myCors $
  Servant.serve (Proxy :: Proxy (RestApi :<|> RouteApi)) $
    RestApi.server dbHandle wsHandle :<|> RouteApi.server
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS"]
