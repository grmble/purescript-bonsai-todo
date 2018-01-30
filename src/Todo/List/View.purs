module Todo.List.View
where


import Prelude hiding (div)

import Bonsai (Cmd, pureCommand)
import Bonsai.EventDecoder (dataAttributeEvent, targetCheckedEvent)
import Bonsai.Html (MarkupT, VNode, (!), (#!), (#!?), attribute, keyedElement, render, text, a, button, caption, div, input, legend, li, table, td, th, thead, tr, ul)
import Bonsai.Html.Attributes (autofocus, checked, cls, colspan, href, id_, name, placeholder, style, target, typ, value)
import Bonsai.Html.Events (onClick, onInput, onKeyEnter, onKeyEnterEscape)
import Bonsai.VirtualDom (on)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Foreign (F, Foreign)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.Traversable (class Foldable, traverse_)
import Data.Tuple (Tuple(..))
import Todo.List.Model (ListMsg(..), sortedEntries)
import Todo.Model (PK, TodoModel, TodoEntry, parsePK)
import Todo.Parser (Task(Task))




listView :: TodoModel -> VNode ListMsg
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
          traverse_ tagView (M.toAscUnfoldable (countTags model) :: Array (Tuple String Int))

  where

    todoTableView :: Tuple PK TodoEntry -> Tuple String (VNode ListMsg)
    todoTableView (Tuple pk entry) =
      lmap (show <<< unwrap)
      case model.editPk of
        Just editPk ->
          if editPk == pk
            then todoTableEdit pk
            else todoTableShow pk entry
        Nothing ->
          todoTableShow pk entry

    todoTableEdit :: PK -> Tuple PK (VNode ListMsg)
    todoTableEdit pk =
      Tuple pk $
        render $
          tr ! attribute "data-pk" (show $ unwrap pk) $ do
            td ! colspan 5 $ do
              input ! cls "pure-input pure-u-1-1"
                ! name "todo-edit"
                ! id_ ("todo-edit-" <> (show $ unwrap pk))
                ! typ "text"
                ! value model.edittodo
                ! onInput EditInput
                ! onKeyEnterEscape (const SaveEdit) (const CancelEdit)

    todoTableShow :: PK -> TodoEntry -> Tuple PK (VNode ListMsg)
    todoTableShow pk entry =
      let
        (Task tsk) =
          entry.task
        markup :: MarkupT ListMsg
        markup =
          tr
            #!? map (\c -> style "background-color" (show c)) entry.highlight
            ! attribute "data-pk" (show $ unwrap pk) $ do
            td $ input !
              typ "checkbox" !
              name "completed" !
              value "y" !
              checked tsk.completed !
              on "change" checkedChangeEvent

            td $ text $ fromMaybe "" tsk.priority
            td $ text tsk.text
            td $ text $ fromMaybe "" tsk.completionDate
            td $ text $ fromMaybe "" tsk.creationDate
      in
        Tuple pk (render markup)

    tagView (Tuple name count) =
      li ! onClick (FilterList name) $ do
        text (name <> "(" <> show count <> ")")



dataPkDecoder :: forall eff. Foreign -> F (Cmd eff ListMsg)
dataPkDecoder =
  -- map pureCommand <<< map StartEdit <<< map parsePK <<< dataAttributeEvent "pk"
  map (pureCommand <<< StartEdit <<< parsePK) <<< dataAttributeEvent "pk"

checkedChangeEvent :: forall eff. Foreign -> F (Cmd eff ListMsg)
checkedChangeEvent ev = do
  b <- targetCheckedEvent ev
  str <- dataAttributeEvent "pk" ev
  pure $ pureCommand $ SetCompleted (parsePK str) b


filteredEntries :: TodoModel -> Array (Tuple PK TodoEntry)
filteredEntries model =
  filter
    (\(Tuple a b) -> contains (Pattern model.filter) b.line)
    (sortedEntries model)


countWords :: forall f. Foldable f => Functor f => f String -> M.Map String Int
countWords words =
  M.fromFoldableWith (+) $ (countAs1 <$> words)
  where
    countAs1 x = Tuple x 1

-- | Counts project and context tags
countTags :: TodoModel -> M.Map String Int
countTags model =
  countWords $ L.concat $ tags <$> (M.values model.todos)
  where
    tags e = L.fromFoldable $ (unwrap e.task).projects <> (unwrap e.task).contexts
