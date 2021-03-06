{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.TodoMVC.Backend.Pure.Todo.App (
  newTodoApp,
  listTodos,
  addTodo,
  removeTodo,
  updateTodo,
  findTodoById,
  clearTodos,
  incrTodoAppCounter,
  lookupTimestamp
) where



import           Control.Lens                        (to, use, (%=), (+=), (.=))
import           Control.Monad.State.Lazy            (gets)
import qualified Data.Map                            as Map (delete, elems,
                                                             empty, insert,
                                                             lookup, update)
import           Data.Time                           (UTCTime)
import           Web.TodoMVC.Backend.Pure.Todo.Types



-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = defaultTodoApp



-- | List todos with potential query params: limit, offset
--
listTodos :: Maybe Int -> Maybe Int -> Maybe TodoState -> TodoAppState TodoResponses
listTodos m_limit m_offset m_filter = do
  todos <- ((applyFilter . Map.elems) <$> gets _todoAppTodos)
  pure (case (m_limit, m_offset) of
    (Just limit, Just offset) -> take limit $ drop offset todos
    (Just limit, Nothing)     -> take limit todos
    (Nothing,    Just offset) -> drop offset todos
    (Nothing,    Nothing)     -> todos)
  where
  applyFilter todos =
    case m_filter of
      Just filt -> filter ((==) filt . _todoResponseState) todos
      Nothing   -> todos



-- | addTodo
--
addTodo :: TodoRequest -> TodoAppState TodoResponse
addTodo TodoRequest{..} = do
  new_id <- incrTodoAppCounter
  m_ts   <- lookupTimestamp
  let new_todo = defaultTodoResponse {
    _todoResponseId        = new_id,
    _todoResponseTitle     = _todoRequestTitle,
    _todoResponseCreatedAt = m_ts
  }
  todoAppTodos %= Map.insert new_id new_todo
  pure new_todo



-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  e <- use (todoAppTodos . to (Map.lookup tid))
  maybe (pure Nothing) (const del) e

  where
  del = (todoAppTodos %= Map.delete tid) *> (pure . pure) tid



-- | updateTodo
--
updateTodo :: TodoId -> TodoRequest -> TodoAppState (Maybe TodoResponse)
updateTodo tid TodoRequest{..} = do
  m_todo <- findTodoById tid
  m_ts   <- lookupTimestamp
  maybe (pure Nothing) (const $ update m_ts) m_todo

  where
  alter_todo m_ts todo_response@TodoResponse{..} =
    Just $ todo_response{
      _todoResponseTitle      = _todoRequestTitle,
      _todoResponseState      = _todoRequestState,
      _todoResponseModifiedAt = m_ts
    }
  update m_ts = do
    todoAppTodos %= Map.update (alter_todo m_ts) tid
    findTodoById tid



-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe TodoResponse)
findTodoById tid = Map.lookup tid <$> gets _todoAppTodos



-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = (todoAppTodos .= Map.empty) *> pure True



-- | incrTodoAppCounter
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  todoAppCounter += 1
  gets _todoAppCounter



-- | lookup time stamp
--
lookupTimestamp :: TodoAppState (Maybe UTCTime)
lookupTimestamp = gets _todoAppTimestamp
