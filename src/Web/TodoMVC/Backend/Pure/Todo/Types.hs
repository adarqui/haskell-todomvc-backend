{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.TodoMVC.Backend.Pure.Todo.Types (
  TodoRequest (..),
  TodoResponse (..),
  TodoResponses,
  TodoActionRequest (..),
  TodoActionResponse (..),
  TodoState (..),
  TodoApp (..),
  TodoAppState,
  TodoId,
  todoRequestTitle,
  todoResponseId,
  todoResponseTitle,
  todoResponseState,
  todoResponseCreatedAt,
  todoResponseModifiedAt,
  todoAppCounter,
  todoAppTodos
) where



import           Control.DeepSeq     (NFData)
import           Control.Lens        (makeLenses)
import           Control.Monad.State (State)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Int            (Int64)
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)



type TodoId = Int64



data TodoState
  = Active
  | Editing
  | Completed
  deriving (Show, Eq, Ord, Generic, Typeable, NFData)

instance FromJSON TodoState
instance ToJSON TodoState



data TodoRequest = TodoRequest {
  _todoRequestTitle :: !Text
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

makeLenses ''TodoRequest

instance FromJSON TodoRequest
instance ToJSON TodoRequest



data TodoResponse = TodoResponse {
  _todoResponseId         :: !TodoId,
  _todoResponseTitle      :: !Text,
  _todoResponseState      :: !TodoState,
  _todoResponseCreatedAt  :: !(Maybe UTCTime),
  _todoResponseModifiedAt :: !(Maybe UTCTime)
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

makeLenses ''TodoResponse

instance FromJSON TodoResponse
instance ToJSON TodoResponse

type TodoResponses = [TodoResponse]



data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo      TodoRequest
  | ReqRemoveTodo   TodoId
  | ReqUpdateTodo   TodoId TodoRequest
  | ReqFindTodoById TodoId
  | ReqClearTodos
  deriving (Show, Eq, Ord, Generic, Typeable, NFData)



data TodoActionResponse
  = RespListTodos    TodoResponses
  | RespAddTodo      (Maybe TodoResponse)
  | RespRemoveTodo   (Maybe TodoId)
  | RespUpdateTodo   (Maybe TodoResponse)
  | RespFindTodoById (Maybe TodoResponse)
  | RespClearTodos   Bool
  deriving (Show, Eq, Ord, Generic, Typeable, NFData)



data TodoApp = TodoApp {
  _todoAppTodos   :: !(Map TodoId TodoResponse),
  _todoAppCounter :: !TodoId
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

makeLenses ''TodoApp



type TodoAppState a = State TodoApp a
