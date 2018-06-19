{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module RouteApi
    ( RouteApi
    , server
    ) where


import           Data.Aeson ()
import           Data.Aeson.TH ()
import           Data.Text (Text)
import qualified Page
import           Servant
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html4.Strict (Markup)


type RouteApi =
  "static" :> Raw
  :<|> CaptureAll "segments" Text :> Get '[HTML] Markup



server :: Server RouteApi
server = staticHandler :<|> pageHandler
  where
    pageHandler _ =
      return Page.index

    staticHandler =
      Servant.serveDirectoryWebApp "../../static"