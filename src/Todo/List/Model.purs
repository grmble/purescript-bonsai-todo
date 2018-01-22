module Todo.List.Model
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (execState)
import Data.Array (filter, sortBy)
import Data.DateTime (DateTime)
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, null, split, trim)
import Data.Tuple (Tuple, snd)
import Todo.CssColor (CssColor)
import Todo.Model (PK, TodoEntry, TodoModel, createEntry)
import Todo.Storage (STORAGE, setItem)

data ListMsg
  = Create String
  -- without NewInput, the animations will clear the input for new todos
  -- so you cannot type when the animations are running
  | NewInput String
  | FilterList String
  | SetHighlight (Maybe CssColor) PK
  | StartEdit PK
  | SaveEdit
  | CancelEdit
  | SetCompleted PK Boolean
  | SetEntryDate PK DateTime

importEntries :: TodoModel -> String -> TodoModel
importEntries model str =
  execState (traverse_ createEntry lines) model
  where
  lines =
      filter (not <<< null)
        (trim <$> split (Pattern "\n") str)

exportEntries :: TodoModel -> String
exportEntries model =
  joinWith "\r\n" $ map (_.line <<< snd) (sortedEntries model)

sortedEntries :: TodoModel -> Array (Tuple PK TodoEntry)
sortedEntries model =
  sortBy order (M.toUnfoldable model.todos)
  where
    order a b = (snd a).line `compare` (snd b).line

-- | Store the todo list to local storage
storeModel :: forall eff. TodoModel -> Aff (console::CONSOLE,storage::STORAGE|eff) Unit
storeModel model = liftEff $ do
  log "storing model"
  setItem "bonsai-todo" (exportEntries model)
