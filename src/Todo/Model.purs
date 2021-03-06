module Todo.Model
where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (State, get, gets, modify, put, state)
import Data.Array.NonEmpty as NEA
import Data.DateTime (DateTime)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Formatter.DateTime as FD
import Data.Int (fromString)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Todo.CssColor (CssColor)
import Todo.Parser (Task(..), parseTodoTxt, todoTxt)

newtype PK =
  PK Int

derive instance newtypePK :: Newtype PK _
derive instance ordPK :: Ord PK
derive instance eqPK :: Eq PK

parsePK :: String -> PK
parsePK str =
  PK $ fromMaybe (-1) $ fromString str

type TodoModel =
  { maxPk   :: Int
  , todos   :: M.Map PK TodoEntry
  , filter  :: String
  , newtodo :: String
  , editPk  :: Maybe PK
  , edittodo:: String
  }


type TodoEntry =
  { task      :: Task   -- parsed from todoTxt
  , line      :: String -- unparsed todoTxt line
  , pk        :: PK
  , highlight :: Maybe CssColor -- maybe current highlight for color animation
  }

emptyTodoModel :: TodoModel
emptyTodoModel = { maxPk: 0, todos: M.fromFoldable [], filter: "", newtodo: "", editPk: Nothing, edittodo: "" }

nextPK :: State TodoModel PK
nextPK = do
  state \model ->
    let pk' = model.maxPk + 1
    in Tuple (PK pk') model { maxPk = pk' }

putEntry :: TodoEntry -> State TodoModel Unit
putEntry entry =
  modify (\m -> m { todos = M.insert entry.pk entry m.todos }) *> pure unit



modifyEntry :: (TodoEntry -> TodoEntry) -> PK -> State TodoModel Unit
modifyEntry fn pk =
  modify (\m -> m { todos = M.update (Just <<< fn) pk m.todos }) *> pure unit


getEntry :: PK -> State TodoModel (Maybe TodoEntry)
getEntry pk = do
  gets (\m -> M.lookup pk m.todos)


createEntry :: String -> State TodoModel PK
createEntry str = do
  pk <- nextPK
  let task = parseTodoTxt str
  let entry = { task, pk, line: todoTxt task, highlight: Nothing }
  putEntry entry
  pure entry.pk


isoDateFmt :: L.List FD.FormatterCommand
isoDateFmt =
  L.fromFoldable
    [ FD.YearFull
    , (FD.Placeholder "-")
    , FD.MonthTwoDigits
    , (FD.Placeholder "-")
    , FD.DayOfMonthTwoDigits
    ]

setEntryDate :: DateTime -> PK -> State TodoModel Unit
setEntryDate d =
  modifyEntry \e ->
    let
      dstr = FD.format isoDateFmt d
      task = unwrap e.task
      task' =
        if task.completed
          then task { completionDate = Just dstr }
          else task { creationDate = task.creationDate <|> Just dstr }
    in
      e { task = Task task', line = todoTxt $ Task task' }

startEdit :: PK -> State TodoModel Unit
startEdit pk = do
  me <- getEntry pk
  for_ me \e -> do
    m <- get
    put m { edittodo = e.line, editPk = Just pk }

saveEdit :: State TodoModel PK
saveEdit = do
  m <- get
  for_ m.editPk
    (modifyEntry \e ->
      let tsk = parseTodoTxt m.edittodo
      in  e { task = tsk , line = todoTxt tsk })
  cancelEdit

cancelEdit :: State TodoModel PK
cancelEdit = do
  pk <- gets _.editPk
  _ <- modify \m -> m { editPk = Nothing, edittodo = ""}
  pure $ unsafePartial $ fromJust pk

highlightEntry :: Maybe CssColor -> PK -> State TodoModel Unit
highlightEntry col =
  modifyEntry \e -> e { highlight = col }


priRegex :: Regex
priRegex =
  unsafePartial $ fromRight $ R.regex """(.*) pri:([A-Z])$"""  noFlags

completeTask :: PK -> State TodoModel Unit
completeTask =
  modifyEntry $ \e ->
    let t = unwrap e.task
    in  case t.priority of
          Just s ->
            e { task = wrap (t { completed = true, text = t.text <> " pri:" <> s }) }
          Nothing ->
            e { task = wrap (t { completed = true }) }

uncompleteTask :: PK -> State TodoModel Unit
uncompleteTask =
  modifyEntry $ \e ->
    let t = unwrap e.task
    in  case NEA.toArray <$> R.match priRegex t.text of
          Just [_, Just txt, Just pri] ->
            e { task = wrap (t  { completed = false
                                , completionDate = Nothing
                                , text = txt
                                , priority = Just pri }) }
          _ ->
            e { task = wrap (t { completed = false, completionDate = Nothing }) }

completeEntry :: Boolean -> PK -> State TodoModel PK
completeEntry b pk = do
  if b
    then completeTask pk
    else uncompleteTask pk
  flip modifyEntry pk $ \e ->
    e { line = todoTxt e.task }
  pure pk
