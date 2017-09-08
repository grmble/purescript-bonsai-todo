module Todo.Edit where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, plainResult, property, text)
import Bonsai.Event (onClick, onInput, onEnter)
import Data.Maybe (Maybe(..), fromMaybe)

type EditModel =
  { todo  :: String
  , reset :: String
  , pk    :: Maybe Int
  , dirty :: Boolean
  }

emptyEditModel :: EditModel
emptyEditModel =
  { todo: ""
  , reset: ""
  , pk: Nothing
  , dirty: false
  }

data EditMsg
  -- local component messages
  = Ok
  | Reset
  | Changed String
  -- requests that current edit model is saved to list
  | SaveEditEntry EditModel

editView :: EditModel -> VNode EditMsg
editView model = -- traceMsg "view" $
  node "div"
    [ attribute "class" "pure-form pure-u-1-1" ]
    [ node "fieldset" []
      [ node "legend" []
        [ text "What would you like "
        , node "a" [ attribute "href" "https://github.com/todotxt/todotxt/"
                   , attribute "target" "_blank" ]
          [ text "to do"]
        , text "?"
        ]
      , node "input"
        [ attribute "class" "pure-u-2-3 pure-input"
        , attribute "name" "todo"
        , attribute "type" "text"
        , attribute "placeholder" "Todo"

        -- note "property" value
        -- if you use attribute, the value will be set in the DOM
        -- but the field will not update.
        -- property *will* update the field
        , property "value" model.todo
        , onInput (pure <<< Changed)
        , onEnter (pure Ok)
        ]
        [ ]
      , node "div" [ attribute "class" "pure-u-1-12" ] []
      , node "button"
        [ attribute "type" "submit"
        , attribute "class" "pure-u-1-12 pure-button pure-button-primary"
        , attribute "name" "ok"
        , onClick (pure Ok)
        ]
        [ text $ fromMaybe "Add" $ const "Update" <$> model.pk ]
      , node "button"
        [ attribute "type" "reset"
        , attribute "class" "pure-u-1-12 pure-button"
        , attribute "name" "cancel"
        , onClick (pure Reset)
        ]
        [ text "Reset" ]
      -- , node "p" [] [ text ("Input value: " <> model.todo) ]
      ]
    ]

editUpdate :: EditModel -> EditMsg -> UpdateResult EditModel EditMsg
editUpdate model msg = -- traceMsg "editUpdate" $
  case msg of
    Ok ->
      { model: emptyEditModel
      , cmd: pure $ SaveEditEntry model
      }
    Reset ->
      plainResult $ model { todo = model.reset, dirty = false }
    Changed todo ->
      plainResult $ model { todo = todo, dirty = true }
    SaveEditEntry _ ->
      plainResult model
