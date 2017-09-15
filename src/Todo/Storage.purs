-- | Simple wrapper for window.localStorage
module Todo.Storage
  ( STORAGE
  , getItem
  , setItem
  , removeItem
  )
where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

-- | Effect type for local storage
foreign import data STORAGE :: Effect



-- | Get a string value from storage
getItem :: forall eff. String -> Eff (storage::STORAGE|eff) (Maybe String)
getItem key =
  pure $ runFn3 getItemFn3 key (toForeign Nothing) (toForeign Just)

foreign import getItemFn3 :: Fn3 String Foreign Foreign (Maybe String)

-- | Store a string value under a key.
foreign import setItem :: forall eff. String -> String -> Eff (storage::STORAGE|eff) Unit

-- | Remove the stored value.
foreign import removeItem :: forall eff. String -> Eff (storage::STORAGE|eff) Unit
