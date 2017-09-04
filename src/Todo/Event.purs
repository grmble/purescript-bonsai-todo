module Todo.Event
  ( targetValue
  , targetFormValues
  , targetValues
  , handleErrors
  , mapNothing
  )
where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (range, catMaybes)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (F, Foreign, ForeignError, readInt, readString,
  renderForeignError, isNull, isUndefined)
import Data.Foreign.Index ((!))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | Read the value of the target input element
targetValue :: Foreign -> F String
targetValue value =
  value ! "target" ! "value" >>= readString

-- ! Read the names and values of the target element's form.
targetFormValues :: Foreign -> F (StrMap String)
targetFormValues value =
  value ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValues :: Foreign -> F (StrMap String)
targetValues value = do
  value ! "target" >>= namesAndValues

-- | Read a strmap of values from a (fake) array of input elements
namesAndValues :: Foreign -> F (StrMap String)
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (fromFoldable <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

nameAndValue :: Foreign -> Int -> F (Maybe (Tuple String String))
nameAndValue arr idx = do
  name <- arr ! idx ! "name"
  value <- arr ! idx ! "value"
  if (isNullOrUndefined name) || (isNullOrUndefined value)
    then pure Nothing
    else do
      n <- readString name
      v <- readString value
      pure (Just (Tuple n v))

isNullOrUndefined :: Foreign -> Boolean
isNullOrUndefined value =
  (isNull value) || (isUndefined value)

-- | Convert the errors from F to EventDecoder
handleErrors
  :: forall a
  .  (Foreign -> F a)
  -> Foreign
  -> Either String a
handleErrors decoder value =
  mapLeft
    combineErrors
    (runExcept $ decoder value)

mapLeft
  :: forall a b c
  .  (a -> c)
  -> Either a b
  -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

mapNothing
  :: forall a
  .  String
  -> Maybe a
  -> Either String a
mapNothing s Nothing = Left s
mapNothing _ (Just a) = Right a

combineErrors
  :: NonEmptyList ForeignError
  -> String
combineErrors errors =
  intercalate ", " $ renderForeignError <$> errors
