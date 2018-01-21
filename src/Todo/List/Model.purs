module Todo.List.Model
where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (concat, filter, sortBy)
import Data.Foldable (class Foldable, foldl)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.StrMap (StrMap, fromFoldable, fromFoldableWith, insert, lookup, toArrayWithKey, update, values)
import Data.String (Pattern(..), contains, joinWith, null, split, trim)
import Data.Tuple (Tuple(Tuple), snd)
import Partial.Unsafe (unsafePartial)
import Todo.CssColor (CssColor)
import Todo.Parser (Task(..), parseTodoTxt, todoTxt, unTask)
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
  -- without NewInput, the animations will clear the input for new todos
  -- so you cannot type when the animations are running
  | NewInput String
  | FilterList String
  | SetHighlight (Maybe CssColor) PK
  | StartEdit PK
  | SaveEdit String
  | SetCompleted PK Boolean
  | CancelEdit

emptyListModel :: ListModel
emptyListModel = { maxPk: 0, todos: fromFoldable [], filter: "", newtodo: "", editPk: Nothing, edittodo: "" }

createEntry :: ListModel -> Task -> Tuple PK ListModel
createEntry model task =
  let
    maxPk = model.maxPk + 1
    pk    = show maxPk
    entry = { task, pk: PK (pk), line: todoTxt task, highlight: Nothing }
    todos = insert pk entry model.todos
  in
    Tuple (PK pk) (model { maxPk = maxPk, todos = todos })


isoDateFmt :: L.List FormatterCommand
isoDateFmt =
  L.fromFoldable
    [ YearFull
    , (Placeholder "-")
    , MonthTwoDigits
    , (Placeholder "-")
    , DayOfMonthTwoDigits
    ]


taskWithCreationDate :: forall eff. String -> Eff (now::NOW|eff) Task
taskWithCreationDate str = do

  let (Task taskRec) = parseTodoTxt str
  if isJust taskRec.creationDate
    then pure (Task taskRec)
    else do
      date <- nowDateString
      pure $ Task (taskRec { creationDate = Just date })

nowDateString :: forall eff. Eff (now::NOW|eff) String
nowDateString = do
  date <- extract <$> nowDateTime
  pure $ format isoDateFmt date


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


setCompleted :: ListModel -> PK -> Boolean -> Tuple PK ListModel
setCompleted model (PK pk) b =

  fromMaybe (Tuple (PK pk) model) $ do
    let todos = update doUpdate pk model.todos
    pure (Tuple (PK pk) (model { todos = todos }))

  where

    doUpdate entry =
      let
        Task task = entry.task
        task' =
          if b
            then Task $ task
              { completed = true
              , completionDate = Just $ unsafePerformEff nowDateString }
            else Task $ task { completed = false, completionDate = Nothing }
        entry' = entry { task = task', line = todoTxt task' }
      in
        Just entry'

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
    combine m s = snd $ createEntry m (parseTodoTxt s)
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
