{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Servant.API (
  runServer
) where



import           Data.Monoid                ((<>))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Todo
import           Web.TodoMVC.Servant.Shared



-- | server
--
server :: Store -> Server LnAPI
server store =
       serveDirectory "./html"
  :<|> serveDirectory "./dist"
  :<|> serveDirectory "./bower_components"
  :<|> runApp store listTodos
  :<|> runApp store . addTodo
  :<|> runApp store clearTodos
  :<|> runApp_Maybe store . findTodoById
  :<|> runApp_Maybe store . removeTodo
  :<|> apply2 updateTodo store -- bleh



-- | app
--
app :: Store -> Application
app store = serve todoAPI $ server store



-- | runServer
--
-- runs the API servers on:
-- http://localhost:port
--
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "We will be listening on port " <> show port
  store <- newAppState
  run port $ app store
