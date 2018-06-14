{-# LANGUAGE TemplateHaskell #-}
module Models
  ( User
  , users
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)


data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


users :: [User]
users = 
  [ User 1 "Isaac" "Newton"
  , User 2 "Albert" "Einstein"
  ]
