module Main where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, debugProgram, domElementById, mapResult, node, plainResult, property, text, simpleTask)
import Bonsai.Event (onClick, onInput)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Partial.Unsafe (unsafePartial)
import Todo.List (ListModel, ListMsg, exportEntries, importEntries, listUpdate, listView, storeModel)
import Todo.Storage (STORAGE, getItem)

main :: forall e. Eff (console::CONSOLE,dom::DOM,storage::STORAGE,ref::REF| e) Unit
main = unsafePartial $ do
  stored <- getItem "bonsai-todo"
  Just mainDiv  <- domElementById (ElementId "main")
  _ <- debugProgram mainDiv true true update view (importModel stored)
  pure unit

emptyModel :: Model
emptyModel =
  { listModel:    { maxPk: 0, todos: [], filter: "", newtodo: "" }
  , importExport: Nothing
  }

importModel :: Maybe String -> Model
importModel Nothing =
  emptyModel
importModel (Just str) =
  emptyModel { listModel = importEntries emptyModel.listModel str }

type Model =
  { listModel :: ListModel
  , importExport:: Maybe String
  }

-- subcomponents Edit and List have their own messages
-- i would actually not recommend this, its easier to have only
-- one message type.
-- it remains in here so there is a test app for VirtualDOM event mapping
data Msg
  = MainListMsg ListMsg
  | ImportExportStart
  | ImportExportText String
  | ImportExportEnd


update :: forall aff. Model -> Msg -> UpdateResult (console::CONSOLE,dom::DOM,ref::REF,storage::STORAGE|aff) Model Msg
update model msg =
  case msg of

    MainListMsg listMsg ->
      delegateListMsg listMsg

    ImportExportStart ->
      plainResult model { importExport = Just (exportEntries model.listModel)}

    ImportExportText str ->
      plainResult model { importExport = Just str }

    ImportExportEnd ->
      let model2 = importModel model.importExport
      in  { model: model2
          , cmd: simpleTask (store model2.listModel)
          }

  where

    store listModel = do
      _ <- storeModel listModel
      pure []

    delegateListMsg listMsg =
      mapResult
        (model { listModel = _ })
        MainListMsg
        (listUpdate model.listModel listMsg)


view :: Model -> VNode Msg
view model =
  case model.importExport of
    Nothing -> viewTodo model
    Just _  -> viewImportExport model

viewTodo :: Model -> VNode Msg
viewTodo model =
  node "div"
    [ ]
    [ MainListMsg <$> listView model.listModel
    , node "button"
        [ attribute "class" "pure-button"
        , onClick ImportExportStart ]
        [ text "Import/Export" ]
    ]

viewImportExport :: Model -> VNode Msg
viewImportExport model =
  node "div"
    [ attribute "class" "pure-g"]
    [ node "div" [ attribute "class" "pure-u-1-1"]
      [ node "textarea"
          [ attribute "class" "pure-input pure-u-1-1"
          , attribute "rows" "25"
          , property "value" (fromMaybe "" model.importExport)
          , onInput ImportExportText]
          []
      , node "button"
          [ onClick ImportExportEnd ]
          [ text "OK" ]
      ]
    ]
