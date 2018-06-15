{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module RestApi
    ( RestApi
    , server
    ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.ByteString.Char8 (pack)
import           Data.Text (Text)
import qualified Db
import           Servant


type RestApi = 
  "todos" :> (
    Get '[JSON] [Db.Task]
    :<|> ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.Task
    :<|> Capture "id" Db.TaskId :> Delete '[JSON] [Db.Task]
    :<|> Capture "id" Db.TaskId :> Get '[JSON] (Maybe Db.Task)
  )


server :: Db.Handle -> Server RestApi
server handle = hoistServer (Proxy :: Proxy RestApi) toHandle todoHandlers
  where
    todoHandlers =
      getAllHandler :<|> updateHandler :<|> newHandler :<|> deleteHandler :<|> queryHandler

    getAllHandler =
      Db.listTasks

    updateHandler task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask task
      throwError $ redirect (Db.id task)

    newHandler txt = do
      tId <- Db.insertTask txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show tId
      throwError $ redirect tId

    deleteHandler tId = do
      Db.deleteTask tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      throwError redirectAll
  
    queryHandler tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      Db.getTask tId

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r handle


redirect :: Db.TaskId -> ServantErr
redirect = addLoc err303
  where
    addLoc err tId =
      let headers = ("Location", pack ("/todos/" ++ show tId)) : errHeaders err
      in err { errHeaders = headers }


redirectAll :: ServantErr
redirectAll = addLoc err303
  where
    addLoc err =
      let headers = ("Location", "/todos") : errHeaders err
      in err { errHeaders = headers }
      