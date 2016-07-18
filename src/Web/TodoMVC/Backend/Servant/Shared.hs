{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Backend.Servant.Shared (
  Store,
  LnAPI,
  todoAPI,
  newAppState,
  runApp,
  runApp_Maybe,
  apply2
) where



import Data.Time (getCurrentTime)
import           Control.Concurrent.STM        (TVar, atomically, newTVarIO,
                                                readTVar, writeTVar)
import           Control.Lens                  (makeLenses, set)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.State           (State, runState)
import           Control.Monad.Trans.Except    (ExceptT)
import           Servant
import           Web.TodoMVC.Backend.Pure.Todo (TodoApp (..), TodoId, TodoRequest,
                                                TodoResponse, TodoResponses,
                                                newTodoApp)



data AppState = AppState {
  _todoApp :: !TodoApp
}

makeLenses ''AppState



type Store = TVar AppState



-- | Todo API Operations
-- GET    /todos
-- POST   /todos
-- DELETE /todos
-- GET    /todos/:todo_id
-- DELETE /todos/:todo_id
-- PUT    /todos/:todo_id
type LnAPI =
       "html"   :> Raw
  :<|> "dist"   :> Raw
  :<|> "static" :> Raw
  :<|> "todos"  :> QueryParam "limit" Int :> QueryParam "offset" Int :>  Get '[JSON] TodoResponses
  :<|> "todos"  :> ReqBody '[JSON] TodoRequest :> Post '[JSON] TodoResponse
  :<|> "todos"  :> Delete '[JSON] Bool
  :<|> "todos"  :> Capture "todo_id" TodoId :> Get '[JSON] TodoResponse
  :<|> "todos"  :> Capture "todo_id" TodoId :> Delete '[JSON] TodoId
  :<|> "todos"  :> Capture "todo_id" TodoId :> ReqBody '[JSON] TodoRequest :> Put '[JSON] TodoResponse



todoAPI :: Proxy LnAPI
todoAPI = Proxy



-- | newAppState
--
newAppState :: IO (TVar AppState)
newAppState = newTVarIO $ AppState newTodoApp



-- | runApp
--
-- simple todo application helper
--
runApp :: MonadIO m => Store -> State TodoApp b -> ExceptT ServantErr m b
runApp store cb = do
  ts <- liftIO getCurrentTime
  liftIO $ atomically $ do
    app_state <- readTVar store
    let
      app_state_with_ts = app_state{_todoApp = (_todoApp app_state){ _todoAppTimestamp = Just ts}}
    let (a, s) = runState cb (_todoApp app_state_with_ts)
    writeTVar store (set todoApp s app_state_with_ts)
    pure a



-- | runApp_Maybe
--
-- pures an error if our todo action pures Nothing
--
runApp_Maybe :: MonadIO m => Store -> State TodoApp (Maybe b) -> ExceptT ServantErr m b
runApp_Maybe store cb = runApp store cb >>= maybe (throwError err400) pure



-- | apply2
--
-- bleh: having some weird type errors
--
apply2 :: MonadIO m => (t -> t1 -> State TodoApp (Maybe b)) -> Store -> t -> t1 -> ExceptT ServantErr m b
apply2 f s x y = runApp_Maybe s (f x y)
