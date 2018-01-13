module Main where

import Prelude hiding (div)

import Bonsai (BONSAI, ElementId(..), UpdateResult, debugProgram, emittingTask, mapResult, plainResult, window)
import Bonsai.Html (VNode, (!), button, div, render, text, textarea, vnode)
import Bonsai.Html.Attributes (cls, rows, value)
import Bonsai.Html.Events (onClick, onInput)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..), fromMaybe)
import Todo.List.Controller (listUpdate)
import Todo.List.Model (ListModel, ListMsg, emptyListModel, exportEntries, importEntries, storeModel)
import Todo.List.View (listView)
import Todo.Storage (STORAGE, getItem)

main :: forall e. Eff (bonsai::BONSAI,exception::EXCEPTION,storage::STORAGE| e) Unit
main = do
  stored <- getItem "bonsai-todo"
  _ <- window >>=
    debugProgram (ElementId "main") update view (importModel stored) true true
  pure unit

emptyModel :: Model
emptyModel =
  { listModel:    emptyListModel
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


update :: forall aff. Model -> Msg -> UpdateResult (console::CONSOLE,bonsai::BONSAI,storage::STORAGE|aff) Model Msg
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
          , cmd: emittingTask \_ ->
              store model2.listModel *> pure unit
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
  render $ div $ do
    vnode (MainListMsg <$> listView model.listModel)
    button ! cls "pure-button" ! onClick ImportExportStart $ do
      text "Import/Export"

viewImportExport :: Model -> VNode Msg
viewImportExport model =
  render $
    div ! cls "pure-g" $
      div ! cls "pure-u-1-1" $ do
        textarea
          ! cls "pure-input pure-u-1-1"
          ! rows 25
          ! value (fromMaybe "" model.importExport)
          ! onInput ImportExportText
        button
          ! onClick ImportExportEnd $
          text "OK"
