module Todo.Edit where

import Prelude

import Bonsai (UpdateResult, VNode, attribute, node, text, plainResult, property)
import Bonsai.Event (onInput, onClickWithOptions, preventDefaultStopPropagation)
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
  node "form"
    [ attribute "class" "pure-form pure-u-1-1"
    -- , onSubmit (const $ pure Ok)
    ]
    [ node "fieldset" []
      [ node "legend" [] [ text "What do you want to do?" ]
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
        , onInput \todo -> pure $ Changed todo ]
        [ ]
      , node "div" [ attribute "class" "pure-u-1-12" ] []
      , node "button"
        [ attribute "type" "submit"
        , attribute "class" "pure-u-1-12 pure-button pure-button-primary"
        , attribute "name" "ok"
        , onClickWithOptions preventDefaultStopPropagation (const $ pure Ok)
        ]
        [ text $ fromMaybe "Add" $ const "Update" <$> model.pk ]
      , node "button"
        [ attribute "type" "reset"
        , attribute "class" "pure-u-1-12 pure-button"
        , attribute "name" "cancel"
        , onClickWithOptions preventDefaultStopPropagation (const $ pure Reset) ]
        [ text "Reset" ]
      , node "p" [] [ text ("Input value: " <> model.todo) ]
      ]
    ]

-- cmdSubmit :: EventDecoder EditMsg
-- cmdSubmit event = do
--   inputs <- handleErrors targetValues event
--   value <- mapNothing "No input named 'todo'." (lookup "todo" inputs)
--   Right [Changed value]


-- traceObj x = traceAny x (const x)
-- traceMsg s x = traceAny s (const x)


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
