module Main where

import Prelude

import Bonsai (program)
import Bonsai.DOM (domElementById)
import Bonsai.VirtualDom (VNode, node, attribute)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Partial.Unsafe (unsafePartial)
import Todo.Edit (EditModel, EditMsg, editUpdate, editView)
import Todo.List (ListModel, ListMsg, listUpdate, listView)

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

update :: Model -> Msg -> Model
update model msg = -- traceMsg "update" $
  case msg of
    MainEditMsg editMsg ->
      model { editModel = editUpdate model.editModel editMsg }
    MainListMsg listMsg ->
      model { listModel = listUpdate model.listModel listMsg }

view :: Model -> VNode Msg
view model =
  node "div"
    [ attribute "class" "pure-g" ]
    [ MainEditMsg <$> editView model.editModel
    , MainListMsg <$> listView model.listModel
    ]
