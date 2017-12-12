module Todo.List.Controller
where

import Prelude

import Bonsai (Cmd, UpdateResult, emitMessages, plainResult, emittingTask, simpleTask)
import Bonsai.DOM (affElementAction, focusCmd, focusSelectCmd)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML.HTMLElement (focus)
import DOM.Node.Types (ElementId(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Todo.CssColor (CssColor(..), gradient)
import Todo.List.Model (ListModel, ListMsg(..), PK, cancelEdit, createEntry, runPK, saveEdit, setHighlight, startEdit, storeModel)
import Todo.Storage (STORAGE)
import Unsafe.Coerce (unsafeCoerce)

listUpdate
  :: forall aff
  .  ListModel
  -> ListMsg
  -> UpdateResult (console::CONSOLE,dom::DOM,ref::REF,storage::STORAGE|aff) ListModel ListMsg
listUpdate model msg =
  case msg of
    Create str ->
      uncurry storeFocusAndAnimate
        (map (\m -> m { newtodo = "" }) (createEntry model str))

    NewInput str ->
      plainResult $ model { newtodo = str }

    FilterList str ->
      plainResult $ model { filter = str }

    SetHighlight color pk ->
      plainResult $ setHighlight color model pk

    StartEdit pk ->
      { model: startEdit model pk
      , cmd:   focusSelectCmd (ElementId ("todo-edit-" <> runPK pk))
      }

    SaveEdit str ->
      uncurry storeFocusAndAnimate $ saveEdit model str

    CancelEdit ->
      { model: cancelEdit model
      , cmd:   focusCmd (ElementId "todo-create")
      }

  where
    storedResult m =
      { model: m, cmd: simpleTask (storeModel m) }

    storeFocusAndAnimate pk m =
      { model: m, cmd: storeFocusAndAnimateCmd pk m }

storeFocusAndAnimateCmd
  :: forall aff
  .  PK
  -> ListModel
  -> Cmd (console::CONSOLE,dom::DOM,storage::STORAGE|aff) ListMsg
storeFocusAndAnimateCmd pk m =
  emittingTask \ctx -> do
    _ <- storeModel m
    -- set the focus to the element todo-create
    affElementAction (focus <<< unsafeCoerce) (ElementId "todo-create")
    for_ (gradient 16 highlightStartColor highlightEndColor) $ \col -> do
      emitMessages ctx [SetHighlight (Just col) pk]
      delay (Milliseconds 200.0)
    emitMessages ctx [SetHighlight Nothing pk]


highlightStartColor :: CssColor
highlightStartColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xc0 }
highlightEndColor :: CssColor
highlightEndColor = CssColor { red: 0xFF, green: 0xFF, blue: 0xFF }
