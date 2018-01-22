module Todo.List.Controller
where

import Prelude

import Bonsai (BONSAI, Cmd, UpdateResult, emitMessage, emittingTask, plainResult)
import Bonsai.DOM (ElementId(..), affElementAction, focusCmd, focusElement, focusSelectCmd)
import Control.Comonad (extract)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.State (execState, runState)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), uncurry)
import Todo.CssColor (CssColor(..), gradient)
import Todo.List.Model (ListMsg(..), storeModel)
import Todo.Model (PK, TodoModel, cancelEdit, completeEntry, createEntry, highlightEntry, saveEdit, setEntryDate, startEdit)
import Todo.Storage (STORAGE)

listUpdate
  :: forall aff
  .  TodoModel
  -> ListMsg
  -> UpdateResult (console::CONSOLE,bonsai::BONSAI,now::NOW,storage::STORAGE|aff) TodoModel ListMsg
listUpdate model msg =
  case msg of

    Create str ->
      let
        Tuple pk model' = runState (createEntry str) model
      in
        uncurry storeFocusAndAnimate (Tuple pk (model' { newtodo = "" }))

    NewInput str ->
      plainResult $ model { newtodo = str }

    FilterList str ->
      plainResult $ model { filter = str }

    SetHighlight color pk ->
      plainResult $ execState (highlightEntry color pk) model

    StartEdit pk ->
      { model: execState (startEdit pk) model
      , cmd:   focusSelectCmd (ElementId ("todo-edit-" <> show (unwrap pk)))
      }

    EditInput s ->
      plainResult $ model { edittodo = s }

    SaveEdit ->
      uncurry storeFocusAndAnimate $ runState saveEdit model

    SetCompleted pk b ->
      uncurry storeFocusAndAnimate $ runState (completeEntry b pk) model

    CancelEdit ->
      { model: execState cancelEdit model
      , cmd:   focusCmd (ElementId "todo-create")
      }

    SetEntryDate pk d ->
      plainResult $ execState (setEntryDate d pk) model


  where
    storeFocusAndAnimate pk m =
      { model: m, cmd: storeFocusAndAnimateCmd pk m }

storeFocusAndAnimateCmd
  :: forall aff
  .  PK
  -> TodoModel
  -> Cmd (console::CONSOLE,bonsai::BONSAI,now::NOW,storage::STORAGE|aff) ListMsg
storeFocusAndAnimateCmd pk m =
  emittingTask \ctx -> do
    date <- liftEff $ extract <$> nowDateTime
    emitMessage ctx (SetEntryDate pk date)
    _ <- storeModel m
    -- set the focus to the element todo-create
    -- affElementAction P.focusElement id ctx.document
    affElementAction focusElement (ElementId "todo-create") ctx.document
    for_ (gradient 16 highlightStartColor highlightEndColor) $ \col -> do
      emitMessage ctx $ SetHighlight (Just col) pk
      delay (Milliseconds 200.0)
    emitMessage ctx $ SetHighlight Nothing pk


highlightStartColor :: CssColor
highlightStartColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xc0 }
highlightEndColor :: CssColor
highlightEndColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xFF }
