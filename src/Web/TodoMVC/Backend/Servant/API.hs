{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Backend.Servant.API (
  runServer
) where



import           Data.Monoid                          ((<>))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Web.TodoMVC.Backend.Pure.Todo        (addTodo, clearTodos,
                                                       findTodoById, listTodos,
                                                       removeTodo, updateTodo)
import           Web.TodoMVC.Backend.Servant.Shared   (LnAPI, Store, apply2,
                                                       newAppState, runApp,
                                                       runApp_Maybe, todoAPI)



-- | server
--
server :: Store -> Server LnAPI
server store =
       serveDirectory "./html"
  :<|> serveDirectory "./dist"
  :<|> serveDirectory "./static"
  :<|> ((runApp store .) .) . listTodos
  :<|> runApp store . addTodo
  :<|> runApp store clearTodos
  :<|> runApp_Maybe store . findTodoById
  :<|> runApp_Maybe store . removeTodo
  :<|> apply2 updateTodo store -- bleh



-- | app
--
app :: Store -> Application
app = serve todoAPI . server



-- | runServer
--
-- runs the API servers on:
-- http://localhost:port
--
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "We will be listening on port " <> show port
  store <- newAppState
  run port $ logStdoutDev $ app store
