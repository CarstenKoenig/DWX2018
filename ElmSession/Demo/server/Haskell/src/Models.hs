{-# LANGUAGE TemplateHaskell #-}
module Models
  ( TaskId
  , Task (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Int (Int64)
import Data.Text (Text)


type TaskId = Int64


data Task = Task { id       :: TaskId
                 , text     :: Text
                 , finished :: Bool
                 } deriving Show

$(deriveJSON defaultOptions ''Task)
