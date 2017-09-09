module Main where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, domElementById, mapResult, node, program, property, text, plainResult)
import Bonsai.Event (onClick, onInput)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Partial.Unsafe (unsafePartial)
import Todo.Edit (EditModel, EditMsg(..), editUpdate, editView, emptyEditModel)
import Todo.List (ListModel, ListMsg(..), exportEntries, importEntries, listUpdate, listView)

main :: forall e. Eff (console::CONSOLE,dom::DOM,ref::REF| e) Unit
main = unsafePartial $ do
  Just mainDiv  <- domElementById (ElementId "main")
  _ <- program mainDiv update view emptyModel
  pure unit

emptyModel :: Model
emptyModel =
  { editModel:    emptyEditModel
  , listModel:    { maxPk: 0, todos: [], filter: ""}
  , importExport: Nothing
  }

importModel :: Maybe String -> Model
importModel Nothing =
  emptyModel
importModel (Just str) =
  emptyModel { listModel = importEntries emptyModel.listModel str }

type Model =
  { editModel :: EditModel
  , listModel :: ListModel
  , importExport:: Maybe String
  }

-- subcomponents Edit and List have their own messages
-- i would actually not recommend this, its easier to have only
-- one message type.
-- it remains in here so there is a test app for VirtualDOM event mapping
data Msg
  = MainEditMsg EditMsg
  | MainListMsg ListMsg
  | ImportExportStart
  | ImportExportText String
  | ImportExportEnd

saveEditEntryMsg :: EditModel -> ListMsg
saveEditEntryMsg editModel =
  Create editModel

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

    ImportExportStart ->
      plainResult model { importExport = Just (exportEntries model.listModel)}

    ImportExportText str ->
      plainResult model { importExport = Just str }

    ImportExportEnd ->
      plainResult (importModel model.importExport)

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
  case model.importExport of
    Nothing -> viewTodo model
    Just _  -> viewImportExport model

viewTodo :: Model -> VNode Msg
viewTodo model =
  node "div"
    [ ]
    [ MainEditMsg <$> editView model.editModel
    , MainListMsg <$> listView model.listModel
    , node "button"
        [ attribute "class" "pure-button"
        , onClick (pure ImportExportStart) ]
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
          , onInput (pure <<< ImportExportText)]
          []
      , node "button"
          [ onClick $ pure ImportExportEnd ]
          [ text "OK" ]
      ]
    ]
