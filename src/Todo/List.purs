module Todo.List
where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, keyedNode, node, plainResult, property, pureCommand, readerTask, simpleTask, style, text)
import Bonsai.Event (onClick, onInput, onKeyEnter)
import Bonsai.Types (TaskContext)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM (DOM)
import Data.Array (concat, filter, findIndex, modifyAt, snoc, sortBy, updateAt)
import Data.Foldable (class Foldable, foldl, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap, fromFoldableWith, toArrayWithKey)
import Data.String (Pattern(..), contains, joinWith, null, split, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd, uncurry)
import Todo.CssColor (CssColor(..), gradient)
import Todo.Parser (Task(..), parseTodoTxt, unTask)
import Todo.Storage (STORAGE, setItem)


type ListModel =
  { maxPk   :: Int
  , todos   :: Array ListEntry
  , filter  :: String
  , newtodo :: String
  }

type ListEntry =
  { task      :: Task   -- parsed from todoTxt
  , line      :: String -- unparsed todoTxt line
  , pk        :: Int
  , highlight :: Maybe CssColor
  }

data ListMsg
  = Create String
  | Update ListEntry
  | Delete ListEntry
  | FilterList String
  | NewTodo String
  | CreateNewTodo String
  | SetHighlight (Maybe CssColor) Int

createEntryNoSort' :: ListModel -> String -> Tuple Int ListModel
createEntryNoSort' model str =
  let
    maxPk = model.maxPk + 1
    entry = { task: parseTodoTxt str, pk: maxPk, line: str, highlight: Nothing }
    todos = snoc model.todos entry
  in
    Tuple maxPk (model { maxPk = maxPk, todos = todos })

createEntryNoSort :: ListModel -> String -> ListModel
createEntryNoSort model str =
  snd $ createEntryNoSort' model str

createEntry :: ListModel -> String -> Tuple Int ListModel
createEntry model str =
  map sortEntries (createEntryNoSort' model str)

updateEntry :: ListModel -> ListEntry -> ListModel
updateEntry model entry =
  fromMaybe model $ do
    idx <- findIndex (\e -> e.pk == entry.pk) model.todos
    todos <- updateAt idx entry model.todos
    pure $ sortEntries $ model { todos = todos }

setHighlight :: Maybe CssColor -> ListModel -> Int -> ListModel
setHighlight highlight model pk =
  fromMaybe model $ do
    idx <- findIndex (\e -> e.pk == pk) model.todos
    todos <- modifyAt idx (\entry -> entry { highlight = highlight }) model.todos
    pure $ model { todos = todos }

deleteEntry :: ListModel -> ListEntry -> ListModel
deleteEntry model entry =
  model { todos = filter (\x -> x.pk /= entry.pk) model.todos }

sortEntries :: ListModel -> ListModel
sortEntries model =
  model { todos = sortBy order model.todos }
  where
    order a b = a.line `compare` b.line

filteredEntries :: ListModel -> Array ListEntry
filteredEntries model =
  filter
    (\entry -> contains (Pattern model.filter) entry.line)
    model.todos

importEntries :: ListModel -> String -> ListModel
importEntries model str =
  sortEntries $
    foldl createEntryNoSort model lines
  where
    lines =
      filter (not <<< null)
        (trim <$> split (Pattern "\n") str)

exportEntries :: ListModel -> String
exportEntries model =
  joinWith "\r\n" $ _.line <$> model.todos

countWords :: forall f. Foldable f => Functor f => f String -> StrMap Int
countWords words =
  fromFoldableWith (+) $ (countAs1 <$> words)
  where
    countAs1 x = Tuple x 1

-- | Counts project and context tags
countTags :: ListModel -> StrMap Int
countTags model =
  countWords $ concat $ tags <$> model.todos
  where
    tags e = (unTask e.task).projects <> (unTask e.task).contexts

-- | Store the todo list to local storage
storeModel :: forall eff. ListModel -> Aff (console::CONSOLE,storage::STORAGE|eff) (Array ListMsg)
storeModel listModel = liftEff $ do
  log "storing model"
  setItem "bonsai-todo" (exportEntries listModel)
  pure []

listView :: ListModel -> VNode ListMsg
listView model =
  -- not a form!  form input handling (ESC!) considered harmful
  node "div" [ attribute "class" "pure-g" ]
    [ node "legend" [ attribute "class" "pure-u-1-1" ]
        [ text "What would you like "
        , node "a" [ attribute "href" "https://github.com/todotxt/todotxt/"
                   , attribute "target" "_blank" ]
          [ text "to do"]
        , text "?"
        ]

    , node "div"
      [ attribute "class" "pure-u-5-6 pure-form" ]
      [ node "input"
        [ attribute "class" "pure-input pure-u-1-1"
        , attribute "autofocus" "autofocus"
        , attribute "name" "todo"
        , attribute "type" "text"
        , attribute "placeholder" "Todo"
        -- property, not attribute !!!
        -- with attribute the field won't change
        , property "value" model.newtodo
        , onInput NewTodo
        , onKeyEnter CreateNewTodo
        ]
        [ ]

      , node "table" [ attribute "class" "pure-table" ] $
          [ node "caption" [] [ text "Your todo-list" ]
          , node "thead" []
            [ node "tr" [ ]
                [ node "th" [ attribute "class" "col-done"] [ text "Done" ]
                , node "th" [ attribute "class" "col-prio"] [ text "Prio" ]
                , node "th" [ attribute "class" "col-todo"] [ text "Todo" ]
                , node "th" [ attribute "class" "col-comp"] [ text "Completed" ]
                , node "th" [ attribute "class" "col-crea"] [ text "Created" ]
                ]
            ]
          , keyedNode "tbody" []
            (map todoTableView (filteredEntries model))
          ]
      ]
    , node "div"
        [ attribute "class" "pure-u-1-6" ]
        [ node "div" [ style [Tuple "padding-left" "2em"] ]
          [ node "button"
              [ onClick $ FilterList "" ]
              [ text "Reset filter" ]
          , node "input"
              [ attribute "class" "pure-input"
              , attribute "name" "filter"
              , attribute "type" "text"
              , attribute "placeholder" "Filter"
              , property "value" model.filter
              , onInput FilterList
              ]
              [ ]
          , node "ul" [ attribute "class" "tag-list" ]
            (map tagView (toArrayWithKey Tuple (countTags model)))
          ]
        ]
    ]

  where

    todoTableView entry =
      let
        (Task tsk) =
          entry.task
        rowAttrs =
          case entry.highlight of
            Nothing ->
              [ ]
            Just color ->
              [
              style [(Tuple "background-color" (show color))]
              ]
        vnode =
          node "tr" rowAttrs $
            [ node "td" [] [ text $ if tsk.completed then "x" else "" ]
            , node "td" [] [ text $ fromMaybe "" tsk.priority ]
            , node "td" [] [ text tsk.text ]
            , node "td" [] [ text $ fromMaybe "" tsk.completionDate ]
            , node "td" [] [ text $ fromMaybe "" tsk.creationDate ]
            ]
      in
        Tuple (show entry.pk) vnode

    tagView (Tuple name count) =
      node "li"
        [ onClick (FilterList name) ]
        [ text (name <> "(" <> show count <> ")")]

listUpdate
  :: forall aff
  .  ListModel
  -> ListMsg
  -> UpdateResult (console::CONSOLE,dom::DOM,ref::REF,storage::STORAGE|aff) ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      uncurry storedAnimated $ createEntry model str

    Update entry ->
      plainResult $ updateEntry model entry

    Delete entry ->
      plainResult $ deleteEntry model entry

    FilterList str ->
      plainResult $ model { filter = str }

    NewTodo str ->
      plainResult $ model { newtodo = str }

    CreateNewTodo str ->
      { model: model { newtodo = "" }
      , cmd: pureCommand $ Create str }

    SetHighlight color pk ->
      plainResult $ setHighlight color model pk

  where
    storedResult m =
      { model: m, cmd: simpleTask (storeModel m) }

    storedAnimated pk m =
      { model: m, cmd: readerTask $ storeAndAnimate pk m }

storeAndAnimate
  :: forall eff aff
  .  Int
  -> ListModel
  -> (TaskContext eff (Array ListMsg) -> Aff (console::CONSOLE,storage::STORAGE|aff) (Array ListMsg))
storeAndAnimate pk m = \ctx -> do
  _ <- storeModel m
  for_ (gradient 16 highlightStartColor highlightEndColor) $ \col -> do
    liftEff $ unsafeCoerceEff $ ctx.emitter [SetHighlight (Just col) pk]
    delay (Milliseconds 200.0)
  pure [SetHighlight Nothing pk]


highlightStartColor :: CssColor
highlightStartColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xc0 }
highlightEndColor :: CssColor
highlightEndColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xFF }
