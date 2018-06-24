-- | Simple wrapper for window.localStorage
module Todo.Storage
  ( getItem
  , setItem
  , removeItem
  )
where

import Prelude

import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))


-- | Get a string value from storage
getItem :: String -> Effect (Maybe String)
getItem key =
  pure $ runFn3 getItemFn3 key (unsafeToForeign Nothing) (unsafeToForeign Just)

foreign import getItemFn3 :: Fn3 String Foreign Foreign (Maybe String)

-- | Store a string value under a key.
foreign import setItem :: String -> String -> Effect Unit

-- | Remove the stored value.
foreign import removeItem :: String -> Effect Unit
