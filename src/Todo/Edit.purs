module Todo.Edit where

import Prelude

import Bonsai.VirtualDom (VNode, EventDecoder, attribute, node, on, onWithOptions, text)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Todo.Event (handleErrors, targetValue)

type EditModel =
  { todo  :: String
  , reset :: String
  , pk    :: Maybe Int
  , dirty :: Boolean
  }

data EditMsg
  = Ok
  | Reset
  | Changed String

editView :: EditModel -> VNode EditMsg
editView model = -- traceMsg "view" $
  node "form"
    [ attribute "class" "pure-form pure-u-1-1"
    , xon "submit" cmdOk ]
    [ node "fieldset" []
      [ node "legend" [] [ text "What do you want to do?" ]
      , node "input"
        [ attribute "class" "pure-input-2-3"
        , attribute "name" "todo"
        , attribute "type" "text"
        , attribute "placeholder" "Todo"
        -- initial value!
        , attribute "value" model.reset
        , on "input" cmdChanged ]
        [ ]
      , node "button"
        [ attribute "type" "submit"
        , attribute "class" "pure-button pure-button-primary"
        , attribute "name" "ok"
        -- enter will onsubmit, but not onclick
        -- , on "click" cmdOk
        ]
        [ text "OK" ]
      , node "button"
        [ attribute "type" "reset"
        , attribute "class" "pure-button"
        , attribute "name" "cancel"
        , on "click" cmdReset ]
        [ text "Reset" ]
      ]
    ]
  where
    xon name what = onWithOptions name xOptions what
    xOptions = { preventDefault: true, stopPropagation: true }

cmdChanged :: EventDecoder EditMsg
cmdChanged event = do
  todo <- handleErrors targetValue event
  Right [Changed todo]

cmdReset :: EventDecoder EditMsg
cmdReset _ =
  Right [Reset]

cmdOk :: EventDecoder EditMsg
cmdOk _ =
  Right [Ok]

-- cmdSubmit :: EventDecoder EditMsg
-- cmdSubmit event = do
--   inputs <- handleErrors targetValues event
--   value <- mapNothing "No input named 'todo'." (lookup "todo" inputs)
--   Right [Changed value]


-- traceObj x = traceAny x (const x)
-- traceMsg s x = traceAny s (const x)


editUpdate :: EditModel -> EditMsg -> EditModel
editUpdate model msg = -- traceObj $
  case msg of
    Ok ->
      model { reset = model.todo, dirty = false }
    Reset ->
      model { todo = model.reset, dirty = false }
    Changed todo ->
      model { todo = todo, dirty = true }
