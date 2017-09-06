module Todo.List
where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, plainResult, text)
import Data.Array (filter, findIndex, null, snoc, updateAt)
import Data.Maybe (fromMaybe)

type ListModel =
  { maxPk :: Int
  , todos :: Array ListEntry
  }

type ListEntry =
  { todo :: String
  , pk :: Int
  }

data ListMsg
  = Create String
  | Update ListEntry
  | Delete ListEntry

createEntry :: ListModel -> String -> ListModel
createEntry model str =
  let
    maxPk = model.maxPk + 1
    entry = { todo: str, pk: maxPk }
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
  node "div" [ attribute "class" "pure-u-1-1" ] $
    if null model.todos
      then
        [ node "p" [ attribute "class" "pure-u-1-1" ] [ text "Your todo-list is empty." ] ]
      else
        [ node "p" [ ] [ text "Your todo list is:"]
        , node "ul" [ attribute "class" "pure-u-1-1" ]
          (map listEntryView model.todos)
        ]
  where
    listEntryView entry =
      node "li" [] [ text entry.todo ]

listUpdate :: ListModel -> ListMsg -> UpdateResult ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      plainResult $ createEntry model str

    Update entry ->
      plainResult $ updateEntry model entry

    Delete entry ->
      plainResult $ deleteEntry model entry
