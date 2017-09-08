module Todo.List
where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, plainResult, text, style)
import Data.Array (filter, findIndex, snoc, updateAt)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Todo.Parser (Task, parseTodoTxt, unTask)

type ListModel =
  { maxPk :: Int
  , todos :: Array ListEntry
  }

type ListEntry =
  { task :: Task
  , pk   :: Int
  }

data ListMsg
  = Create String
  | Update ListEntry
  | Delete ListEntry

createEntry :: ListModel -> String -> ListModel
createEntry model str =
  let
    maxPk = model.maxPk + 1
    entry = { task: parseTodoTxt str, pk: maxPk }
    todos = snoc model.todos entry
  in
    { maxPk, todos }

updateEntry :: ListModel -> ListEntry -> ListModel
updateEntry model entry =
  fromMaybe model $ do
    idx <- findIndex (\e -> e.pk == entry.pk) model.todos
    todos <- updateAt idx entry model.todos
    pure $ model { todos = todos }

deleteEntry :: ListModel -> ListEntry -> ListModel
deleteEntry model entry =
  model { todos = filter (\x -> x.pk /= entry.pk) model.todos }

listView :: ListModel -> VNode ListMsg
listView model =
  node "table" [ attribute "class" "pure-u-2-3 pure-table pure-table-striped" ] $
    [ node "colgroup" [ ]
        [ node "col" [ style [ Tuple "width" "5em" ]] []
        , node "col" [ style [ Tuple "width" "*" ]] []
        , node "col" [ style [ Tuple "width" "10em" ]] []
        , node "col" [ style [ Tuple "width" "10em" ]] []
        ]
    , node "thead" []
      [ node "tr" [ ]
          [ node "th" [ attribute "class" "col-prio"] [ text "Prio" ]
          , node "th" [ attribute "class" "col-todo"] [ text "Todo"]
          , node "th" [ attribute "class" "col-comp"] [ text "Completed"]
          , node "th" [ attribute "class" "col-crea"] [ text "Created"]
          ]
      ]
    , node "tbody" []
      (map listEntryView model.todos)
    ]
  where
    listEntryView entry =
      let
        tsk = unTask entry.task
      in
        node "tr" [ ] $
          [ node "td" [] [ text $ fromMaybe "" tsk.priority ]
          , node "td" [] [ text tsk.text ]
          , node "td" [] [ text $ fromMaybe "" tsk.completionDate ]
          , node "td" [] [ text $ fromMaybe "" tsk.creationDate ]
          ]

listUpdate :: ListModel -> ListMsg -> UpdateResult ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      plainResult $ createEntry model str

    Update entry ->
      plainResult $ updateEntry model entry

    Delete entry ->
      plainResult $ deleteEntry model entry
