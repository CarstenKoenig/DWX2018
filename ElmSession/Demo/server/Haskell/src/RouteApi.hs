{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module RouteApi
    ( RouteApi
    , server
    ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Data.Aeson ()
import           Data.Aeson.TH ()
import           Data.ByteString.Char8 (pack)
import           Data.Text (Text)
import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import qualified Page
import           Servant
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html4.Strict (Markup)


type RouteApi =
  Get '[HTML] Markup
  :<|> "static" :> Raw


server :: Server RouteApi
server = pageHandler :<|> staticHandler
  where
    pageHandler =
      return Page.index

    staticHandler =
      Servant.serveDirectoryWebApp "../../static"