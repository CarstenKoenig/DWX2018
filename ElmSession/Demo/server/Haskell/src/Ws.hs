{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Ws
  ( Handle
  , TaskChange (..)
  , initialize
  , broadcast
  , connect
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TChan as STM
import           Control.Monad (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.ByteString.Lazy (ByteString)
import           Models (Task, TaskId)
import qualified Network.WebSockets.Connection as WS


newtype Handle = Handle
  { getChannel :: STM.TChan ByteString }


data TaskChange
    = NewTask Task
    | UpdateTask Task
    | DeleteTask TaskId
  
$(deriveJSON defaultOptions ''TaskChange)
  

initialize :: IO Handle
initialize = do
  chan      <- STM.newBroadcastTChanIO
  return $ Handle chan


broadcast :: MonadIO m => Handle -> TaskChange -> m ()
broadcast handle taskChange = liftIO $ atomically $
  STM.writeTChan (getChannel handle) (encode taskChange)


connect :: forall m . MonadIO m => Handle -> WS.Connection -> m ()
connect handle connection = liftIO $ do
  putStrLn "WebSocket connected"
  chan <- atomically $ STM.dupTChan (getChannel handle)
  WS.forkPingThread connection 10

  forever $ do
    textData <- atomically $ STM.readTChan chan
    WS.sendTextData connection textData