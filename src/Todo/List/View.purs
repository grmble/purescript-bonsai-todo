module Todo.List.View
where


import Prelude hiding (div)

import Bonsai (Cmd, pureCommand)
import Bonsai.EventDecoder (dataAttributeEvent)
import Bonsai.Html (MarkupT, VNode, (!), (#!), (#!?), attribute, keyedElement, render, text, a, button, caption, div, input, legend, li, table, td, th, thead, tr, ul)
import Bonsai.Html.Attributes (autofocus, checked, cls, colspan, href, id_, name, placeholder, style, target, typ, value)
import Bonsai.Html.Events (onCheckedChange, onClick, onInput, onKeyEnter, onKeyEnterEscape)
import Bonsai.Types (f2cmd)
import Bonsai.VirtualDom (on)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (toArrayWithKey)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple))
import Todo.List.Model (ListModel, ListMsg(..), PK(..), ListEntry, countTags, filteredEntries, runPK)
import Todo.Parser (Task(Task))




listView :: ListModel -> VNode ListMsg
listView model =
  -- not a form!  form input handling (ESC!) considered harmful
  render $ div ! cls "pure-g" $ do
    legend ! cls "pure-u-1-1" $ do
      text "What would you like "
      a ! href "https://github.com/todotxt/todotxt/" ! target "_blank" $ do
        text "to do"
      text "?"

    div ! cls "pure-u-5-6 pure-form" $ do
      input
        ! id_ "todo-create"
        ! cls "pure-input pure-u-1-1"
        ! autofocus true
        ! name "todo"
        ! typ "text"
        ! placeholder "Todo"
        ! value model.newtodo
        ! onInput NewInput
        ! onKeyEnter Create

      table ! cls "pure-table" ! on "dblclick" dataPkDecoder $ do
        caption $ text "Your todo-list"
        thead $ do
          tr $ do
            th ! cls "col-done" $ text "✔"
            th ! cls "col-prio" $ text "Pri"
            th ! cls "col-todo" $ text "Todo"
            th ! cls "col-comp" $ text "Completed"
            th ! cls "col-crea" $ text "Created"

        keyedElement "tbody" []
          (map todoTableView (filteredEntries model))


    div ! cls "pure-u-1-6" $ do
      div #! style "padding-left" "2em" $ do
        button ! onClick (FilterList "") $ text "Reset filter"
        input ! cls "pure-input" ! name "filter" ! typ "text" ! placeholder "Filter"
          ! value model.filter ! onInput FilterList

        ul ! cls "tag-list" $ do
          traverse_ tagView (toArrayWithKey Tuple (countTags model))

  where

    todoTableView (Tuple pk entry) =
      case model.editPk of
        Just editPk ->
          if (runPK editPk) == pk
            then todoTableEdit pk
            else todoTableShow pk entry
        Nothing ->
          todoTableShow pk entry

    todoTableEdit pk =
      Tuple pk $
        render $
          tr ! attribute "data-pk" pk $ do
            td ! colspan 5 $ do
              input ! cls "pure-input pure-u-1-1"
                ! name "todo-edit"
                ! id_ ("todo-edit-" <> pk)
                ! typ "text"
                ! value model.edittodo
                ! onKeyEnterEscape SaveEdit (const CancelEdit)

    todoTableShow :: String -> ListEntry -> Tuple String (VNode ListMsg)
    todoTableShow pk entry =
      let
        (Task tsk) =
          entry.task
        markup :: MarkupT ListMsg
        markup =
          tr
            #!? map (\c -> style "background-color" (show c)) entry.highlight
            ! attribute "data-pk" pk $ do
            td $ input !
              typ "checkbox" !
              name "completed" !
              value "y" !
              checked tsk.completed !
              onCheckedChange (SetCompleted (PK pk))
            td $ text $ fromMaybe "" tsk.priority
            td $ text tsk.text
            td $ text $ fromMaybe "" tsk.completionDate
            td $ text $ fromMaybe "" tsk.creationDate
      in
        Tuple pk (render markup)

    tagView (Tuple name count) =
      li ! onClick (FilterList name) $ do
        text (name <> "(" <> show count <> ")")



dataPkDecoder :: forall eff. Foreign -> Either Error (Cmd eff ListMsg)
dataPkDecoder =
  (f2cmd pureCommand <<< map StartEdit <<< map PK <<< dataAttributeEvent "pk")
