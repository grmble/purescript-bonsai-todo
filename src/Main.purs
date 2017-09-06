module Main where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, domElementById, mapResult, node, program)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Todo.Edit (EditModel, EditMsg(..), editUpdate, editView)
import Todo.List (ListModel, ListMsg(..), listUpdate, listView)

main :: forall e. Eff (console::CONSOLE,dom::DOM,ref::REF| e) Unit
main = unsafePartial $ do
  Just mainDiv  <- domElementById (ElementId "main")
  prog <- program mainDiv update view emptyModel
  log "..."

emptyModel :: Model
emptyModel =
  { editModel: { todo: "", reset: "", dirty: false, pk: Nothing }
  , listModel: { maxPk: 0, todos: [] }
  }

type Model =
  { editModel :: EditModel
  , listModel :: ListModel
  }

data Msg
  = MainEditMsg EditMsg
  | MainListMsg ListMsg

saveEditEntryMsg :: EditModel -> ListMsg
saveEditEntryMsg editModel =
  case editModel.pk of
    Nothing -> Create editModel.todo
    Just pk -> Update { pk: pk, todo: editModel.todo }

update :: Model -> Msg -> UpdateResult Model Msg
update model msg = -- traceMsg "update" $
  case msg of

    MainEditMsg editMsg ->

      case editMsg of

        SaveEditEntry entry ->
          delegateListMsg $ saveEditEntryMsg entry

        _ ->
          delegateEditMsg editMsg

    MainListMsg listMsg ->
      delegateListMsg listMsg

  where

    delegateListMsg listMsg =
      mapResult
        (model { listModel = _ })
        MainListMsg
        (listUpdate model.listModel listMsg)

    delegateEditMsg editMsg =
        mapResult
          (model { editModel = _ })
          MainEditMsg
          (editUpdate model.editModel editMsg)


view :: Model -> VNode Msg
view model =
  node "div"
    [ attribute "class" "pure-g" ]
    [ MainEditMsg <$> editView model.editModel
    , MainListMsg <$> listView model.listModel
    ]
