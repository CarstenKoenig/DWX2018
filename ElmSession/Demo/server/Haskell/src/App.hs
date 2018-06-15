{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module App
    ( startApp
    , app
    ) where


import           Control.Monad.IO.Class (liftIO)
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


type API = 
  ( "todos" :> (
    Get '[JSON] [Db.Task]
    :<|> ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.Task
    :<|> Capture "id" Db.TaskId :> Delete '[JSON] [Db.Task]
    :<|> Capture "id" Db.TaskId :> Get '[JSON] (Maybe Db.Task)
    )
  )
  :<|> Get '[HTML] Markup
  :<|> "static" :> Raw


getPort :: IO Int
getPort = return 8080


getDbPath :: IO FilePath
getDbPath = return "./todos.db"


startApp :: IO ()
startApp = do
  dbPath <- getDbPath
  putStrLn $ "initializing Database in " ++ dbPath
  handle <- Db.initDb dbPath

  port <- getPort
  putStrLn $ "starting Server on " ++ show port
  run port $ app handle


app :: Db.Handle -> Application
app = Servant.serve api . server 


api :: Proxy API
api = Proxy


server :: Db.Handle -> Server API
server handle = 
  todoHandlers
  :<|> pageHandler
  :<|> staticHandler
  where
    todoHandlers =
      getAllHandler :<|> updateHandler :<|> newHandler :<|> deleteHandler :<|> queryHandler

    getAllHandler =
      Db.listTasks handle

    updateHandler task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask handle task
      throwError $ redirect (Db.id task)

    newHandler txt = do
      tId <- Db.insertTask handle txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show tId
      throwError $ redirect tId

    deleteHandler tId = do
      Db.deleteTask handle tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      throwError redirectAll
  

    queryHandler tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      Db.getTask handle tId

    pageHandler = 
      return Page.index

    staticHandler =
      Servant.serveDirectoryWebApp "../../static"


redirect :: Db.TaskId -> ServantErr
redirect tId = addLoc tId err303
  where
    addLoc tId err =
      let headers = ("Location", pack ("/todos/" ++ show tId)) : errHeaders err
      in err { errHeaders = headers }

redirectAll :: ServantErr
redirectAll = addLoc err303
  where
    addLoc err =
      let headers = ("Location", "/todos") : errHeaders err
      in err { errHeaders = headers }
      