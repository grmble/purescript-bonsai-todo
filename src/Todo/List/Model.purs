module Todo.List.Model
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concat, filter, sortBy)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.StrMap (StrMap, fromFoldable, fromFoldableWith, insert, lookup, toArrayWithKey, update, values)
import Data.String (Pattern(..), contains, joinWith, null, split, trim)
import Data.Tuple (Tuple(Tuple), snd)
import Partial.Unsafe (unsafePartial)
import Todo.CssColor (CssColor)
import Todo.Parser (Task, parseTodoTxt, unTask)
import Todo.Storage (STORAGE, setItem)


type ListModel =
  { maxPk   :: Int
  , todos   :: StrMap ListEntry
  , filter  :: String
  , newtodo :: String
  , editPk  :: Maybe PK
  , edittodo:: String
  }

newtype PK =
  PK String

runPK :: PK -> String
runPK (PK pk) = pk

type ListEntry =
  { task      :: Task   -- parsed from todoTxt
  , line      :: String -- unparsed todoTxt line
  , pk        :: PK
  , highlight :: Maybe CssColor -- maybe current highlight for color animation
  }

data ListMsg
  = Create String
  | FilterList String
  | SetHighlight (Maybe CssColor) PK
  | StartEdit PK
  | SaveEdit String
  | CancelEdit

emptyListModel :: ListModel
emptyListModel = { maxPk: 0, todos: fromFoldable [], filter: "", newtodo: "", editPk: Nothing, edittodo: "" }

createEntry :: ListModel -> String -> Tuple PK ListModel
createEntry model str =
  let
    maxPk = model.maxPk + 1
    pk    = show maxPk
    entry = { task: parseTodoTxt str, pk: PK (pk), line: str, highlight: Nothing }
    todos = insert pk entry model.todos
  in
    Tuple (PK pk) (model { maxPk = maxPk, todos = todos })

startEdit :: ListModel -> PK -> ListModel
startEdit model (PK pk) =
  fromMaybe model $ do
    entry <- lookup pk model.todos
    pure model { edittodo = entry.line, editPk = Just (PK pk) }

saveEdit :: ListModel -> String -> Tuple PK ListModel
saveEdit model str =
  unsafePartial $ fromJust $ do
    PK pk <- model.editPk
    let todos = update doUpdate pk model.todos
    pure (Tuple (PK pk) (model { todos = todos, editPk = Nothing, edittodo = "" }))
  where
    doUpdate entry = Just entry { task = parseTodoTxt str, line = str }

cancelEdit :: ListModel -> ListModel
cancelEdit model =
  model { editPk = Nothing, edittodo = "" }

setHighlight :: Maybe CssColor -> ListModel -> PK -> ListModel
setHighlight highlight model (PK pk) =
  fromMaybe model $ do
    let todos = update doUpdate pk model.todos
    pure model { todos = todos }
  where
    doUpdate entry = Just entry { highlight = highlight }


sortEntries :: ListModel -> Array (Tuple String ListEntry)
sortEntries model =
  sortBy order (toArrayWithKey Tuple  model.todos)
  where
    order a b = (snd a).line `compare` (snd b).line

filteredEntries :: ListModel -> Array (Tuple String ListEntry)
filteredEntries model =
  filter
    (\(Tuple a b) -> contains (Pattern model.filter) b.line)
    (sortEntries model)

importEntries :: ListModel -> String -> ListModel
importEntries model str =
  foldl combine model lines
  where
    combine model str = snd $ createEntry model str
    lines =
      filter (not <<< null)
        (trim <$> split (Pattern "\n") str)

exportEntries :: ListModel -> String
exportEntries model =
  joinWith "\r\n" $ map (_.line <<< snd) (sortEntries model)

countWords :: forall f. Foldable f => Functor f => f String -> StrMap Int
countWords words =
  fromFoldableWith (+) $ (countAs1 <$> words)
  where
    countAs1 x = Tuple x 1

-- | Counts project and context tags
countTags :: ListModel -> StrMap Int
countTags model =
  countWords $ concat $ tags <$> (values model.todos)
  where
    tags e = (unTask e.task).projects <> (unTask e.task).contexts

-- | Store the todo list to local storage
storeModel :: forall eff. ListModel -> Aff (console::CONSOLE,storage::STORAGE|eff) (Array ListMsg)
storeModel listModel = liftEff $ do
  log "storing model"
  setItem "bonsai-todo" (exportEntries listModel)
  pure []
