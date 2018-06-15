{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Db
  ( TaskId
  , Task (..)
  , initDb
  , withConnection
  , getTask
  , listTasks
  , insertTask
  , deleteTask
  , modifyTask
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Models


initDb :: MonadIO m => FilePath -> m ()
initDb file = withConnection file $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS todos (\
    \task NVARCHAR(255) NOT NULL, \
    \finished BOOLEAN NOT NULL )"


withConnection :: MonadIO m => FilePath -> (Sql.Connection -> IO a) -> m a
withConnection file m = 
  liftIO $ Sql.withConnection file m


getTask :: TaskId -> Sql.Connection -> IO (Maybe Task)
getTask tId conn =
  listToMaybe . map toTask <$> Sql.query conn "SELECT task,finished FROM todos WHERE rowid = ?" (Sql.Only tId)
  where
    toTask (txt,fin) = Task tId txt fin


listTasks :: Sql.Connection -> IO [Task]
listTasks conn = 
  map toTask <$> Sql.query_ conn "SELECT rowid,task,finished FROM todos"
  where
    toTask (tId,txt,fin) = Task tId txt fin


insertTask :: Text -> Sql.Connection -> IO TaskId
insertTask txt conn = do
  Sql.execute conn "INSERT INTO todos (task,finished) VALUES (?,0)" (Sql.Only txt)
  Sql.lastInsertRowId conn


deleteTask :: TaskId -> Sql.Connection -> IO ()
deleteTask tId conn =
  Sql.execute conn "DELETE FROM todos WHERE rowid=?" (Sql.Only tId)


modifyTask :: Task -> Sql.Connection -> IO ()
modifyTask (Task tId txt fin) conn =
  Sql.executeNamed conn "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id" [":id" := tId, ":task" := txt, ":finished" := fin]