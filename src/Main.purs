module Main where

import Prelude hiding (div)

import Bonsai (Cmd, ElementId(..), debugProgram, noDebug, unitTask, window)
import Bonsai.Html (VNode, (!), button, div, render, text, textarea, vnode)
import Bonsai.Html.Attributes (cls, rows, value)
import Bonsai.Html.Events (onClick, onInput)
import Effect (Effect)
import Control.Plus (empty)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Todo.List.Controller (listUpdate)
import Todo.List.Model (ListMsg, exportEntries, importEntries, storeModel)
import Todo.List.View (listView)
import Todo.Model (TodoModel, emptyTodoModel)
import Todo.Storage (getItem)

main :: Effect Unit
main = do
  stored <- getItem "bonsai-todo"
  _ <- dbgProgram (ElementId "main") update view (importModel stored) window
  pure unit

  where
    dbgProgram =
      debugProgram (noDebug { timing = true })

emptyModel :: Model
emptyModel =
  { todoModel: emptyTodoModel
  , importExport: Nothing
  }

importModel :: Maybe String -> Model
importModel Nothing =
  emptyModel
importModel (Just str) =
  emptyModel { todoModel = importEntries emptyModel.todoModel str }

type Model =
  { todoModel :: TodoModel
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


update
  :: Msg
  -> Model
  -> Tuple (Cmd Msg) Model
update msg model =
  case msg of

    MainListMsg listMsg ->
      bimap (map MainListMsg) (model { todoModel = _ })
        (listUpdate listMsg model.todoModel)

    ImportExportStart ->
      Tuple empty $ model { importExport = Just (exportEntries model.todoModel)}

    ImportExportText str ->
      Tuple empty $ model { importExport = Just str }

    ImportExportEnd ->
      let model2 = importModel model.importExport
      in  Tuple (unitTask $ const $ storeModel model2.todoModel) model2



view :: Model -> VNode Msg
view model =
  case model.importExport of
    Nothing -> viewTodo model
    Just _  -> viewImportExport model

viewTodo :: Model -> VNode Msg
viewTodo model =
  render $ div $ do
    vnode (MainListMsg <$> listView model.todoModel)
    div ! cls "l-box pure-u-1-3 pure-u-md-1-6" $
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
