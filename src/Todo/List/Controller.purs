module Todo.List.Controller
where

import Prelude

import Bonsai (BONSAI, Cmd, emitMessage, emittingTask)
import Bonsai.Core.DOM (focusCmd, focusSelectCmd)
import Bonsai.DOM (DOM, ElementId(..), affF, elementById, focusElement)
import Control.Comonad (extract)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, nowDateTime)
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
import Todo.Storage (STORAGE)

listUpdate
  :: forall aff
  .  ListMsg
  -> TodoModel
  -> Tuple (Cmd (console::CONSOLE,bonsai::BONSAI,dom::DOM,now::NOW,storage::STORAGE|aff) ListMsg) TodoModel
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
  :: forall aff
  .  PK
  -> TodoModel
  -> Cmd (console::CONSOLE,bonsai::BONSAI,dom::DOM,now::NOW,storage::STORAGE|aff) ListMsg
storeFocusAndAnimateCmd pk m =
  emittingTask \ctx -> do
    date <- liftEff $ extract <$> nowDateTime
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
