module Todo.List.View
where


import Prelude hiding (div)

import Bonsai (Cmd)
import Bonsai.EventHandlers (dataAttribute, dataAttributeHandler, onWithOptions, targetChecked)
import Bonsai.Html (Markup, VNode, a, attribute, button, caption, div, input, keyed, keyedElement, legend, li, render, table, td, text, th, thead, tr, ul, (!), (#!?))
import Bonsai.Html.Attributes (autofocus, checked, cls, colspan, defaultValue, href, id_, name, placeholder, style, target, typ, value)
import Bonsai.Html.Events (onClick, onInput, onKeyEnter, onKeyEnterEscape, preventDefaultStopPropagation)
import Bonsai.VirtualDom (on)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Foreign (F, Foreign)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains, toLower)
import Data.Traversable (class Foldable, traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Todo.List.Model (ListMsg(..), sortedEntries)
import Todo.Model (PK, TodoModel, TodoEntry, parsePK)
import Todo.Parser (Task(Task))




listView :: TodoModel -> VNode ListMsg
listView model =
  -- not a form!  form input handling (ESC!) considered harmful
  render $ do
    legend ! cls "l-box-lr pure-u-1" $ do
      text "What would you like "
      a ! href "https://github.com/todotxt/todotxt/" ! target "_blank" $ do
        text "to do"
      text "?"

    div ! cls "l-box pure-u-1 pure-u-md-5-6 pure-form" $ do
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

      table ! cls "pure-table" ! on "dblclick" dataPkHandler $ do
        caption $ text "Your todo-list"
        thead $ do
          tr $ do
            th ! cls "col-done" $ text "✔"
            th ! cls "col-prio" $ text "Pri"
            th ! cls "col-todo" $ text "Todo"
            th ! cls "col-comp" $ text "Completed"
            th ! cls "col-crea" $ text "Created"

        keyedElement "tbody" $
          traverse_ (uncurry keyed) (map todoTableView (filteredEntries model))


    div ! cls "l-box pure-u-1-3 pure-u-md-1-6" $ do
      div  $ do
        button ! onClick (FilterList "") $ text "Reset filter"
        input ! cls "pure-input pure-u-1" ! name "filter" ! typ "text" ! placeholder "Filter"
          ! value model.filter ! onInput FilterList

        ul ! cls "tag-list" $ do
          traverse_ tagView (M.toUnfoldable (countTags model) :: Array (Tuple String Int))

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
        markup :: Markup ListMsg
        markup =
          tr
            #!? map (\c -> style "background-color" (show c)) entry.highlight
            ! attribute "data-pk" (show $ unwrap pk) $ do
            td $ input !
              typ "checkbox" !
              name "completed" !
              -- value properties always show up in the diff, but commenting out
              -- does not make a difference
              defaultValue "y" !
              checked tsk.completed !
              onWithOptions preventDefaultStopPropagation "change" checkedChangeEvent

            td $ text $ fromMaybe "" tsk.priority
            td $ text tsk.text
            td ! cls "col-comp" $ text $ fromMaybe "" tsk.completionDate
            td ! cls "col-crea" $ text $ fromMaybe "" tsk.creationDate
      in
        Tuple pk (render markup)

    tagView (Tuple name count) =
      li ! onClick (FilterList name) $ do
        text (name <> "(" <> show count <> ")")



dataPkHandler :: Foreign -> F (Cmd ListMsg)
dataPkHandler =
  dataAttributeHandler (StartEdit <<< parsePK) "pk"

checkedChangeEvent :: Foreign -> F (Cmd ListMsg)
checkedChangeEvent ev = do
  b <- targetChecked ev
  str <- dataAttribute "pk" ev
  pure $ pure $ SetCompleted (parsePK str) b


filteredEntries :: TodoModel -> Array (Tuple PK TodoEntry)
filteredEntries model =
  filter mustShow $ sortedEntries model

  where
    pat =
      (Pattern $ toLower model.filter)
    mustShow (Tuple a b) =
      contains pat (toLower b.line)


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
