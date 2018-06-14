{-# LANGUAGE OverloadedStrings #-}

module Page
  ( index
  ) where

import           Text.Blaze.Html4.Strict (Markup, (!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A


index :: Markup
index = H.html $ do
  H.head $ do
    H.title "Todo List in Elm"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "static/style.css"
  H.body $
    H.h1 "Hello Blaze!"
