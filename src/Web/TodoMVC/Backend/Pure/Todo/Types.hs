{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.TodoMVC.Backend.Pure.Todo.Types (
  TodoId,
  TodoRequest (..),
  TodoResponse (..),
  TodoResponses,
  TodoState (..),
  TodoApp (..),
  TodoAppState,
  Param (..),
  TodoParam (..),
  todoParamToQueryParamsTuple,
  defaultTodoState,
  defaultTodoRequest,
  defaultTodoResponse,
  defaultTodoApp,
  defaultTodoParam,
  todoRequestTitle,
  todoRequestState,
  todoResponseId,
  todoResponseTitle,
  todoResponseState,
  todoResponseCreatedAt,
  todoResponseModifiedAt,
  todoAppCounter,
  todoAppTodos,
  todoAppTimestamp,
  todoResponseToRequest,
  flipTodoState
) where



import           Control.DeepSeq     (NFData)
import           Control.Lens        (makeLenses)
import           Control.Monad.State (State)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Int            (Int64)
import           Data.Map            (Map)
import qualified Data.Map            as Map (empty)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text as Text (pack)
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import           Web.HttpApiData     (FromHttpApiData, parseQueryParam)



type TodoId = Int64



data TodoState
  = Active
  | Editing
  | Completed
  deriving (Eq, Ord, Generic, Typeable, NFData)

instance FromJSON TodoState
instance ToJSON TodoState

instance Show TodoState where
  show Active    = "active"
  show Editing   = "editing"
  show Completed = "completed"

instance Read TodoState where
  readsPrec _ "active"    = [(Active, "")]
  readsPrec _ "editing"   = [(Editing, "")]
  readsPrec _ "completed" = [(Completed, "")]
  readsPrec _ _           = [(Active, "")]

instance FromHttpApiData TodoState where
  parseQueryParam "active"    = Right Active
  parseQueryParam "editing"   = Right Editing
  parseQueryParam "completed" = Right Completed
  parseQueryParam _           = Left "Unable to parse TodoState"

defaultTodoState :: TodoState
defaultTodoState = Active



data TodoRequest = TodoRequest {
  _todoRequestTitle :: !Text,
  _todoRequestState :: !TodoState
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

makeLenses ''TodoRequest

instance FromJSON TodoRequest
instance ToJSON TodoRequest

defaultTodoRequest :: TodoRequest
defaultTodoRequest = TodoRequest "" Active



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

defaultTodoResponse :: TodoResponse
defaultTodoResponse = TodoResponse 0 "" defaultTodoState Nothing Nothing



data TodoApp = TodoApp {
  _todoAppTodos     :: !(Map TodoId TodoResponse),
  _todoAppCounter   :: !TodoId,
  _todoAppTimestamp :: !(Maybe UTCTime)
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

makeLenses ''TodoApp

type TodoAppState a = State TodoApp a

defaultTodoApp :: TodoApp
defaultTodoApp = TodoApp Map.empty 0 Nothing



data Param
  = Param_Limit  !Int
  | Param_Offset !Int
  | Param_Filter !(Maybe TodoState)
  deriving (Eq, Ord, Generic, Typeable, NFData)

instance Show Param where
  show (Param_Limit limit)   = "limit="  <> show limit
  show (Param_Offset offset) = "offset=" <> show offset
  show (Param_Filter filt)   = "filter=" <> show filt

instance Read Param where
  readsPrec _ "filter=active"    = [(Param_Filter $ Just Active, "")]
  readsPrec _ "filter=completed" = [(Param_Filter $ Just Completed, "")]
  readsPrec _ _                  = [(Param_Filter Nothing, "")]



data TodoParam = TodoParam {
  tpLimit  :: !(Maybe Int),
  tpOffset :: !(Maybe Int),
  tpFilter :: !(Maybe TodoState)
} deriving (Show, Eq, Ord, Generic, Typeable, NFData)

defaultTodoParam :: TodoParam
defaultTodoParam = TodoParam {
  tpLimit  = Nothing,
  tpOffset = Nothing,
  tpFilter = Nothing
}

todoParamToQueryParamsTuple :: TodoParam -> [(Text, Text)]
todoParamToQueryParamsTuple TodoParam{..} =
  limit <> offset <> filt
  where
  limit  = maybe [] (\x -> [("limit", Text.pack $ show x)]) tpLimit
  offset = maybe [] (\x -> [("offset", Text.pack $ show x)]) tpOffset
  filt   = maybe [] (\x -> [("filter", Text.pack $ show x)]) tpFilter




todoResponseToRequest :: TodoResponse -> TodoRequest
todoResponseToRequest TodoResponse{..} = TodoRequest _todoResponseTitle _todoResponseState



flipTodoState :: TodoState -> TodoState
flipTodoState st =
  case st of
    Active    -> Completed
    Completed -> Active
    _         -> st
