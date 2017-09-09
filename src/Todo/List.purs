module Todo.List
where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, keyedNode, plainResult, text, property, style)
import Bonsai.Event (onInput, onClick)
import Data.Array (concat, filter, findIndex, snoc, sortBy, updateAt)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap, fromFoldableWith, toArrayWithKey)
import Data.String (Pattern(..), contains, joinWith, split, trim)
import Data.Tuple (Tuple(..))
import Todo.Parser (Task(..), parseTodoTxt, unTask)


type ListModel =
  { maxPk :: Int
  , todos :: Array ListEntry
  , filter:: String
  }

type ListEntry =
  { task :: Task   -- parsed from todoTxt
  , line :: String -- unparsed todoTxt line
  , pk   :: Int
  }

data ListMsg
  = Create String
  | Update ListEntry
  | Delete ListEntry
  | FilterList String

createEntryNoSort :: ListModel -> String -> ListModel
createEntryNoSort model str =
  let
    maxPk = model.maxPk + 1
    entry = { task: parseTodoTxt str, pk: maxPk, line: str }
    todos = snoc model.todos entry
  in
    model { maxPk = maxPk, todos = todos }

createEntry :: ListModel -> String -> ListModel
createEntry model str =
  sortEntries $ createEntryNoSort model str

updateEntry :: ListModel -> ListEntry -> ListModel
updateEntry model entry =
  fromMaybe model $ do
    idx <- findIndex (\e -> e.pk == entry.pk) model.todos
    todos <- updateAt idx entry model.todos
    pure $ sortEntries $ model { todos = todos }

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

listView :: ListModel -> VNode ListMsg
listView model =
  node "div" [ attribute "class" "pure-g pure-u-1-1" ]
    [ node "table" [ attribute "class" "pure-u-5-6 pure-table pure-table-striped" ] $
        [ node "thead" []
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
    , node "div"
        [ attribute "class" "pure-u-1-6" ]
        [ node "div" [ style [Tuple "padding-left" "2em"] ]
          [ node "input"
              [ attribute "class" "pure-input"
              , attribute "name" "filter"
              , attribute "type" "text"
              , attribute "placeholder" "Filter"
              , property "value" model.filter
              , FilterList <$> onInput
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
        vnode =
          node "tr" [ ] $
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

listUpdate :: ListModel -> ListMsg -> UpdateResult ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      plainResult $ createEntry model str

    Update entry ->
      plainResult $ updateEntry model entry

    Delete entry ->
      plainResult $ deleteEntry model entry

    FilterList str ->
      plainResult $ model { filter = str }
