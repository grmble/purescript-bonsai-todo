module Todo.List.View
where


import Prelude

import Bonsai (Cmd, VNode, attribute, keyedNode, node, property, pureCommand, style, text)
import Bonsai.Event (on, onClick, onInput, onKeyEnter, onKeyEnterEscape, dataAttributeEvent)
import Bonsai.Types (f2cmd)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (toArrayWithKey)
import Data.Tuple (Tuple(Tuple))
import Todo.List.Model (ListModel, ListMsg(..), PK(..), countTags, filteredEntries, runPK)
import Todo.Parser (Task(Task))




listView :: ListModel -> VNode ListMsg
listView model =
  -- not a form!  form input handling (ESC!) considered harmful
  node "div" [ attribute "class" "pure-g" ]
    [ node "legend" [ attribute "class" "pure-u-1-1" ]
        [ text "What would you like "
        , node "a" [ attribute "href" "https://github.com/todotxt/todotxt/"
                   , attribute "target" "_blank" ]
          [ text "to do"]
        , text "?"
        ]

    , node "div"
      [ attribute "class" "pure-u-5-6 pure-form" ]
      [ node "input"
        [ attribute "class" "pure-input pure-u-1-1"
        , attribute "autofocus" "autofocus"
        , attribute "name" "todo"
        , attribute "id"   "todo-create"
        , attribute "type" "text"
        , attribute "placeholder" "Todo"
        -- property, not attribute !!!
        -- with attribute the field won't change
        , property "value" model.newtodo
        -- , onInput NewTodo
        , onKeyEnter Create
        ]
        [ ]

      , node "table"
          [ attribute "class" "pure-table"
          , on "dblclick" dataPkDecoder
          ]
          [ node "caption" [] [ text "Your todo-list" ]
          , node "thead" []
            [ node "tr" [ ]
                [ node "th" [ attribute "class" "col-done"] [ text "Done" ]
                , node "th" [ attribute "class" "col-prio"] [ text "Prio" ]
                , node "th" [ attribute "class" "col-todo"] [ text "Todo" ]
                , node "th" [ attribute "class" "col-comp"] [ text "Completed" ]
                , node "th" [ attribute "class" "col-crea"] [ text "Created" ]
                ]
            ]
          , keyedNode "tbody" []
            (map todoTableView (filteredEntries model))
          ]
      ]

    , node "div"
        [ attribute "class" "pure-u-1-6" ]
        [ node "div" [ style [Tuple "padding-left" "2em"] ]
          [ node "button"
              [ onClick $ FilterList "" ]
              [ text "Reset filter" ]
          , node "input"
              [ attribute "class" "pure-input"
              , attribute "name" "filter"
              , attribute "type" "text"
              , attribute "placeholder" "Filter"
              , property  "value" model.filter
              , onInput FilterList
              ]
              [ ]
          , node "ul" [ attribute "class" "tag-list" ]
            (map tagView (toArrayWithKey Tuple (countTags model)))
          ]
        ]
    ]

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
        node "tr" [ attribute "data-pk" pk ]
          [ node "td" [ attribute "colspan" "5" ]
              [ node "input"
                  [ attribute "class" "pure-input pure-u-1-1"
                  , attribute "name"  "todo-edit"
                  , attribute "id"    ("todo-edit-" <> pk)
                  , attribute "type"  "text"
                  , property  "value" model.edittodo
                  , onKeyEnterEscape SaveEdit (const CancelEdit)
                  ]
                  []
              ]
          ]

    todoTableShow pk entry =
      let
        (Task tsk) =
          entry.task
        rowAttrs =
          case entry.highlight of
            Nothing ->
              [ ]
            Just color ->
              [
              style [(Tuple "background-color" (show color))]
              ]
        vnode =
          node "tr" (rowAttrs <> [ attribute "data-pk" pk ]) $
            [ node "td" [] [ text $ if tsk.completed then "x" else "" ]
            , node "td" [] [ text $ fromMaybe "" tsk.priority ]
            , node "td" [] [ text tsk.text ]
            , node "td" [] [ text $ fromMaybe "" tsk.completionDate ]
            , node "td" [] [ text $ fromMaybe "" tsk.creationDate ]
            ]
      in
        Tuple pk vnode

    tagView (Tuple name count) =
      node "li"
        [ onClick (FilterList name) ]
        [ text (name <> "(" <> show count <> ")")]



dataPkDecoder :: forall eff. Foreign -> Either Error (Cmd eff ListMsg)
dataPkDecoder =
  (f2cmd pureCommand <<< map StartEdit <<< map PK <<< dataAttributeEvent "pk")
