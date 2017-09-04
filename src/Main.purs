module Main where

import Prelude

import Bonsai (program)
import Bonsai.DOM (domElementById)
import Bonsai.VirtualDom (VNode, Options, node, on, text, style, attribute)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Partial.Unsafe (unsafePartial)
import Todo.Edit
import Todo.List
import Data.Tuple (Tuple(..))

main :: forall e. Eff (console::CONSOLE,dom::DOM,ref::REF| e) Unit
main = unsafePartial $ do
  Just mainDiv  <- domElementById (ElementId "main")
  _ <- program mainDiv update view emptyModel
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

update model msg =
  case msg of
    MainEditMsg editMsg ->
      model { editModel = editUpdate model.editModel editMsg }
    MainListMsg listMsg ->
      model { listModel = listUpdate model.listModel listMsg }

view model =
  node "div"
    [ attribute "class" "pure-g" ]
    [ MainEditMsg <$> editView model.editModel
    , MainListMsg <$> listView model.listModel
    ]
