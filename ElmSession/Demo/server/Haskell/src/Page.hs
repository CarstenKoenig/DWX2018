{-# LANGUAGE OverloadedStrings #-}

module Page
  ( index
  ) where

import           Data.Foldable (traverse_)
import           Text.Blaze.Html4.Strict (Markup, (!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A


index :: Markup
index = H.html $ do
  H.head $ do
    H.title "Todo in Elm"
    links
  H.body scripts


links :: H.Html
links =
  traverse_ (\ref -> H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href ref) cssSrcs
  where
    cssSrcs =
      [ "static/lib/bootstrap/dist/css/bootstrap.min.css"
      , "static/lib/fontawesome/dist/fontawesome-all.min.css"
      , "static/style.css"
      ]


scripts :: H.Html
scripts = do
  traverse_ (\ref -> H.script "" ! A.src ref) jsSrcs
  H.script $ H.toHtml $ unlines
    [ "var app = Elm.Main.fullscreen({baseUrl: '/' });" ]

  where
    jsSrcs =
      [ "static/lib/jquery/dist/jquery.js"
      , "static/lib/popper/dist/popper.js"
      , "static/todo.js"
      ]