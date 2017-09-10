module Todo.Edit where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, plainResult, property, style, text)
import Bonsai.Event (onClick, onInput, onEnter)
import Data.Tuple (Tuple(..))

type EditModel = String

emptyEditModel :: EditModel
emptyEditModel = ""

data EditMsg
  -- local component messages
  = Ok
  | Changed String
  -- requests that current edit model is saved to list
  | SaveEditEntry EditModel

editView :: EditModel -> VNode EditMsg
editView model =
  -- not a form!  form input handling (ESC!) considered harmful
  node "div"
    [ attribute "class" "pure-g" ]

    [ node "legend" [ attribute "class" "pure-u-1-1" ]
        [ text "What would you like "
        , node "a" [ attribute "href" "https://github.com/todotxt/todotxt/"
                   , attribute "target" "_blank" ]
          [ text "to do"]
        , text "?"
        ]

    , node "div" [ attribute "class" "pure-u-5-6 pure-form" ]

      [ node "input"
        [ attribute "class" "pure-input pure-u-1-1"
        , attribute "name" "todo"
        , attribute "type" "text"
        , attribute "placeholder" "Todo"

        -- note "property" value
        -- if you use attribute, the value will be set in the DOM
        -- but the field will not update.
        -- property *will* update the field
        , property "value" model
        , Changed <$> onInput
        , onEnter Ok
        ]
        [ ]
      ]

    , node "div"
        [ attribute "class" "pure-u-1-6" ]
        [ node "div" [ style [Tuple "padding-left" "2em"] ]
            [ node "button"
              [ attribute "type" "submit"
              , attribute "class" "pure-button pure-button-primary"
              , attribute "name" "ok"
              , onClick Ok
              ]
              [ text "Add" ]
            ]
        ]
    ]

editUpdate :: EditModel -> EditMsg -> UpdateResult EditModel EditMsg
editUpdate model msg = -- traceMsg "editUpdate" $
  case msg of
    Ok ->
      { model: emptyEditModel
      , cmd: pure $ SaveEditEntry model
      }
    Changed todo ->
      plainResult todo
    SaveEditEntry _ ->
      plainResult model
