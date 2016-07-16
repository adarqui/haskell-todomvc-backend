{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Servant.Shared (
  Store,
  LnAPI,
  todoAPI,
  newAppState,
  runApp,
  runApp_Maybe,
  apply2
) where



import           Control.Concurrent.STM     (TVar, atomically, newTVarIO,
                                             readTVar, writeTVar)
import           Control.Lens               (makeLenses, set)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State        (State, runState)
import           Control.Monad.Trans.Except (ExceptT)
import           Servant
import           Todo                       (Todo, TodoApp, TodoId, newTodoApp)



data AppState = AppState {
  _todoApp :: TodoApp
}

makeLenses ''AppState



type Store = TVar AppState



type LnAPI =
       "html" :> Raw
  :<|> "dist" :> Raw
  :<|> "static" :> Raw
  -- GET /todos
  -- POST /todos , body = Todo
  -- DELETE /todos
  -- GET /todos/:todo_id
  -- DELETE /todos/:todo_id
  -- PUT /todos/:todo_id , body = Todo
  :<|> "todos" :> Get '[JSON] [Todo]
  :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo
  :<|> "todos" :> Delete '[JSON] Bool
  :<|> "todos" :> Capture "todo_id" TodoId :> Get '[JSON] Todo
  :<|> "todos" :> Capture "todo_id" TodoId :> Delete '[JSON] TodoId
  :<|> "todos" :> Capture "todo_id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo



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
  liftIO $ atomically $ do
    v <- readTVar store
    let (a, s) = runState cb (_todoApp v)
    writeTVar store (set todoApp s v)
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
