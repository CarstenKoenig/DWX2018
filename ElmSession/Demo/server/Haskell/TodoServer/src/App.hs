{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module App
    ( startApp
    , app
    ) where

import           Data.Aeson ()
import           Data.Aeson.TH ()
import           Models (User, users)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import qualified Page
import           Servant (Server, JSON, Proxy(..), Raw, Get, (:<|>)(..), (:>))
import qualified Servant
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html4.Strict (Markup)


type API = 
  "users" :> Get '[JSON] [User]
  :<|> Get '[HTML] Markup
  :<|> "static" :> Raw


getPort :: IO Int
getPort = return 8080


startApp :: IO ()
startApp = do
  port <- getPort
  putStrLn $ "starting Server on " ++ show port
  run port app


app :: Application
app = Servant.serve api server


api :: Proxy API
api = Proxy


server :: Server API
server = 
  usersHandler 
  :<|> pageHandler
  :<|> staticHandler
  where
    usersHandler =
      return users

    pageHandler = 
      return Page.index

    staticHandler =
      Servant.serveDirectoryWebApp "../../../static"