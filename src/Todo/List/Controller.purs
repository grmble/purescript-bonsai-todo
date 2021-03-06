module Todo.List.Controller
where

import Prelude

import Bonsai (Cmd, emitMessage, emittingTask)
import Bonsai.Core.DOM (focusCmd, focusSelectCmd)
import Bonsai.DOM (ElementId(..), affF, elementById, focusElement)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Control.Monad.State (execState, runState)
import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), uncurry)
import Todo.CssColor (CssColor(..), gradient)
import Todo.List.Model (ListMsg(..), storeModel)
import Todo.Model (PK, TodoModel, cancelEdit, completeEntry, createEntry, highlightEntry, saveEdit, setEntryDate, startEdit)

listUpdate
  :: ListMsg
  -> TodoModel
  -> Tuple (Cmd ListMsg) TodoModel
listUpdate msg model =
  case msg of

    Create str ->
      let
        Tuple pk model' = runState (createEntry str) model
      in
        uncurry storeFocusAndAnimate (Tuple pk (model' { newtodo = "" }))

    NewInput str ->
      Tuple empty $ model { newtodo = str }

    FilterList str ->
      Tuple empty $ model { filter = str }

    SetHighlight color pk ->
      Tuple empty $ execState (highlightEntry color pk) model

    StartEdit pk ->
      Tuple
        (focusSelectCmd (ElementId ("todo-edit-" <> show (unwrap pk))))
        (execState (startEdit pk) model)

    EditInput s ->
      Tuple empty $ model { edittodo = s }

    SaveEdit ->
      uncurry storeFocusAndAnimate $ runState saveEdit model

    SetCompleted pk b ->
      uncurry storeFocusAndAnimate $ runState (completeEntry b pk) model

    CancelEdit ->
      Tuple
        (focusCmd (ElementId "todo-create"))
        (execState cancelEdit model)

    SetEntryDate pk d ->
      Tuple empty $ execState (setEntryDate d pk) model


  where
    storeFocusAndAnimate pk m =
      Tuple (storeFocusAndAnimateCmd pk m) m

storeFocusAndAnimateCmd
  :: PK
  -> TodoModel
  -> Cmd ListMsg
storeFocusAndAnimateCmd pk m =
  emittingTask \ctx -> do
    date <- liftEffect $ nowDateTime
    emitMessage ctx (SetEntryDate pk date)
    _ <- storeModel m
    -- set the focus to the element todo-create
    -- affElementAction P.focusElement id ctx.document
    affF do
      elem <- elementById (ElementId "todo-create") ctx.document
      focusElement elem *> pure unit
    for_ (gradient 16 highlightStartColor highlightEndColor) $ \col -> do
      emitMessage ctx $ SetHighlight (Just col) pk
      delay (Milliseconds 200.0)
    emitMessage ctx $ SetHighlight Nothing pk


highlightStartColor :: CssColor
highlightStartColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xc0 }
highlightEndColor :: CssColor
highlightEndColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xFF }
