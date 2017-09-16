module Todo.List
where

import Prelude

import Bonsai (Cmd(..), UpdateResult, VNode, attribute, laterCommand, keyedNode, node, plainResult, property, pureCommand, style, text)
import Bonsai.Event (onClick, onInput, onKeyEnter)
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (concat, filter, findIndex, modifyAt, snoc, sortBy, updateAt)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap, fromFoldableWith, toArrayWithKey)
import Data.String (Pattern(..), contains, joinWith, split, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd, uncurry)
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
  , highlight :: Boolean
  }

data ListMsg
  = Create String
  | Update ListEntry
  | Delete ListEntry
  | FilterList String
  | NewTodo String
  | CreateNewTodo String
  | RemoveHighlight Int


createEntryNoSort' :: Boolean -> ListModel -> String -> Tuple Int ListModel
createEntryNoSort' highlight model str =
  let
    maxPk = model.maxPk + 1
    entry = { task: parseTodoTxt str, pk: maxPk, line: str, highlight }
    todos = snoc model.todos entry
  in
    Tuple maxPk (model { maxPk = maxPk, todos = todos })

createEntryNoSort :: Boolean -> ListModel -> String -> ListModel
createEntryNoSort highlight model str =
  snd $ createEntryNoSort' highlight model str

createEntry :: ListModel -> String -> Tuple Int ListModel
createEntry model str =
  map sortEntries (createEntryNoSort' true model str)

updateEntry :: ListModel -> ListEntry -> ListModel
updateEntry model entry =
  fromMaybe model $ do
    idx <- findIndex (\e -> e.pk == entry.pk) model.todos
    todos <- updateAt idx entry model.todos
    pure $ sortEntries $ model { todos = todos }

setHighlight :: Boolean -> ListModel -> Int -> ListModel
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
    foldl (createEntryNoSort false) model lines
  where
    lines =
      trim <$> split (Pattern "\n") str

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
storeModel :: forall eff. ListModel -> Eff (console::CONSOLE,storage::STORAGE|eff) (Array ListMsg)
storeModel listModel = do
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
          if entry.highlight
            then [ attribute "class" "highlight" ]
            else [ ]
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

listUpdate :: forall aff. ListModel -> ListMsg -> UpdateResult (console::CONSOLE,storage::STORAGE|aff) ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      uncurry storedNoHighlight $ createEntry model str

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

    RemoveHighlight pk ->
      plainResult $ setHighlight false model pk

  where
    storedResult model =
      { model: model, cmd: Now (storeModel model) }

    storedNoHighlight pk model =
      { model: model, cmd: laterCommand $ storeAndDelay pk model }
      
    storeAndDelay pk model = do
      _ <- liftEff $ storeModel model
      delay (Milliseconds 5000.0)
      pure (RemoveHighlight pk)
