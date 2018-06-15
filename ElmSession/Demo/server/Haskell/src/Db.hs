{-# LANGUAGE OverloadedStrings #-}

module Db
  ( TaskId
  , Task (..)
  , Handle
  , initDb
  , getTask
  , listTasks
  , insertTask
  , deleteTask
  , modifyTask
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Models


newtype Handle = 
  Handle (MVar FilePath)

initDb :: MonadIO m => FilePath -> m Handle
initDb file = do
  fileVar <- liftIO $ newMVar file
  let handle = Handle fileVar
  useHandle handle $ \conn ->
    Sql.execute_ conn
      "CREATE TABLE IF NOT EXISTS todos (\
      \task NVARCHAR(255) NOT NULL, \
      \finished BOOLEAN NOT NULL )"
  return handle


useHandle :: MonadIO m => Handle -> (Sql.Connection -> IO a) -> m a
useHandle (Handle dbFile) m = liftIO $
    bracket 
      (takeMVar dbFile)
      (putMVar dbFile)
      (`Sql.withConnection` m)


getTask :: MonadIO m => Handle -> TaskId -> m (Maybe Task)
getTask handle tId = useHandle handle $ \conn ->
  listToMaybe . map toTask <$> Sql.query conn "SELECT task,finished FROM todos WHERE rowid = ?" (Sql.Only tId)
  where
    toTask (txt,fin) = Task tId txt fin


listTasks :: MonadIO m => Handle -> m [Task]
listTasks handle = useHandle handle $ \conn ->
  map toTask <$> Sql.query_ conn "SELECT rowid,task,finished FROM todos"
  where
    toTask (tId,txt,fin) = Task tId txt fin


insertTask :: MonadIO m => Handle -> Text -> m TaskId
insertTask handle txt = useHandle handle $ \conn -> do
  Sql.execute conn "INSERT INTO todos (task,finished) VALUES (?,0)" (Sql.Only txt)
  Sql.lastInsertRowId conn


deleteTask :: MonadIO m => Handle -> TaskId -> m ()
deleteTask handle tId = useHandle handle $ \conn ->
  Sql.execute conn "DELETE FROM todos WHERE rowid=?" (Sql.Only tId)


modifyTask :: MonadIO m => Handle -> Task -> m ()
modifyTask handle (Task tId txt fin) = useHandle handle $ \conn ->
  Sql.executeNamed conn "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id" [":id" := tId, ":task" := txt, ":finished" := fin]