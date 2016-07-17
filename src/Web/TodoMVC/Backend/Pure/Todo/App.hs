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
  runTodoGrammar,
  incrTodoAppCounter
) where



import           Control.Lens                        (to, use, (%=), (+=), (.=))
import           Control.Monad.State.Lazy            (gets)
import qualified Data.Map                            as Map (delete, elems,
                                                             empty, insert,
                                                             lookup, update)
import           Web.TodoMVC.Backend.Pure.Todo.Types



-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = defaultTodoApp



-- | listTodos
--
listTodos :: TodoAppState TodoResponses
listTodos = use (todoAppTodos . to Map.elems)



-- | addTodo
--
addTodo :: TodoRequest -> TodoAppState TodoResponse
addTodo TodoRequest{..} = do
  new_id <- incrTodoAppCounter
  let new_todo = defaultTodoResponse { _todoResponseId = new_id, _todoResponseTitle = _todoRequestTitle }
  todoAppTodos %= Map.insert new_id new_todo
  pure new_todo



-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  e <- use (todoAppTodos . to (Map.lookup tid))
  maybe (pure Nothing) (const del) e

  where
  del = todoAppTodos %= Map.delete tid >> (pure . pure) tid



-- | updateTodo
--
updateTodo :: TodoId -> TodoRequest -> TodoAppState (Maybe TodoResponse)
updateTodo tid TodoRequest{..} = do
  m_todo <- findTodoById tid
  maybe (pure Nothing) (const update) m_todo

  where
  alter_todo todo_response@TodoResponse{..} =
    Just $ todo_response{_todoResponseTitle = _todoRequestTitle, _todoResponseState = _todoRequestState}
  update = do
    todoAppTodos %= Map.update alter_todo tid
    findTodoById tid



-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe TodoResponse)
findTodoById tid =  Map.lookup tid <$> gets _todoAppTodos



-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = do
  todoAppTodos .= Map.empty
  pure True



-- | incrTodoAppCounter
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  todoAppCounter += 1
  gets _todoAppCounter



-- | runTodoGrammar
--
-- our todo application grammar in its entirety.
--
runTodoGrammar :: TodoActionRequest -> TodoAppState TodoActionResponse
runTodoGrammar ReqListTodos              = RespListTodos        <$> listTodos
runTodoGrammar (ReqAddTodo todo)         = (RespAddTodo . Just) <$> addTodo todo
runTodoGrammar (ReqRemoveTodo tid)       = RespRemoveTodo       <$> removeTodo tid
runTodoGrammar (ReqUpdateTodo tid todo)  = RespUpdateTodo       <$> updateTodo tid todo
runTodoGrammar (ReqFindTodoById tid)     = RespFindTodoById     <$> findTodoById tid
runTodoGrammar ReqClearTodos             = RespClearTodos       <$> clearTodos
