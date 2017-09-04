module Todo.List
where

import Prelude

import Todo.Edit (EditModel, EditMsg(..))
import Data.Array (snoc, filter, null )
import Bonsai.VirtualDom (VNode, node, text, attribute)

type ListModel =
  { maxPk :: Int
  , todos :: Array ListEntry
  }

type ListEntry =
  { todo :: String
  , pk :: Int
  }

data ListMsg
  = Add String
  | Remove ListEntry
  | Edit ListEntry
  | Update ListEntry


listView :: ListModel -> VNode ListMsg
listView model =
  if null model.todos
    then
      node "p" [ attribute "class" "pure-u-1-1" ] [ text "Your todo-list is empty." ]
    else
      node "ul" [ attribute "class" "pure-u-1-1" ]
        (map listEntryView model.todos)
  where
    listEntryView entry =
      node "ul" [] [ text entry.todo ]

listUpdate :: ListModel -> ListMsg -> ListModel
listUpdate model msg =
  case msg of

    Add str ->
      let
        maxPk = model.maxPk + 1
        entry = { todo: str, pk: maxPk }
        todos = snoc model.todos entry
      in
        { maxPk, todos }

    Remove entry ->
      model { todos = filter (\x -> x.pk /= entry.pk) model.todos }

    Edit entry ->
      model

    Update entry ->
      model
