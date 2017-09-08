module Todo.Parser
  ( Task(..)
  , todoTxt
  , parseTodoTxt
  , unTask
  )
where

import Prelude

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), indexOf, singleton, split)
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (option, optionMaybe)
import Text.Parsing.StringParser.String (regex, string, upperCaseChar)



-- | TodoTxt task, see https://github.com/todotxt/todotxt/
-- |
-- | * priority is ALWAYS first
-- | * if completed, the x is *instead* of priority (it can be preserved as pri:X though)
-- | * if completed, there are two dates (completion and creation) or none
-- | * if not completed, there is an optional creation date
-- | * @Contexts or +Projects
newtype Task =
  Task
  { priority :: Maybe String
  , completed :: Boolean
  , completionDate :: Maybe String
  , creationDate :: Maybe String
  , text :: String
  , projects :: Array String
  , contexts :: Array String
  }

derive instance genericTask :: Generic Task _
instance showTask :: Show Task where
  show = genericShow

task :: Maybe String -> Maybe String -> Maybe String -> String -> Task
task priority completionDate creationDate text =
  Task
    { priority
    , completed: fromMaybe false (const true <$> priority)
    , completionDate
    , creationDate
    , text
    , projects: findWordsStartingWith (Pattern "+") text
    , contexts: findWordsStartingWith (Pattern "@") text
    }

unTask
  :: Task
  -> { priority :: Maybe String
     , completed :: Boolean
     , completionDate :: Maybe String
     , creationDate :: Maybe String
     , text :: String
     , projects :: Array String
     , contexts :: Array String
     }
unTask (Task tsk) =
  tsk

taskParser :: Parser Task
taskParser = do
  done <- completed
  case done of

    true -> do
      dates <- optionMaybe (Tuple <$> date <*> date)
      text <- regex """.*"""
      pure $ task Nothing (fst <$> dates) (snd <$> dates) text

    false -> do
      prio <- priority
      crea <- optionMaybe date
      text <- regex ".*"
      pure $ task prio Nothing crea text

-- Format task in todo txt format
taskWriter :: Task -> Writer String Unit
taskWriter (Task tsk) =
  case tsk.completed of

    true -> do
      tell "x "
      tell $ msp tsk.completionDate
      tell $ msp tsk.creationDate
      tell tsk.text

    false -> do
      tell $ msp (sur <$> tsk.priority)
      tell $ msp tsk.creationDate
      tell tsk.text

  where
    msp m = fromMaybe "" (app <$> m)
    app str = str <> " "
    sur str = "(" <> str <> ")"

-- | Format a task as TodoTxt string
todoTxt :: Task -> String
todoTxt tsk =
  execWriter $ taskWriter tsk

-- | Parse a TodoTxt line
parseTodoTxt :: String -> Task
parseTodoTxt str =
  case runParser taskParser str of
    Right tsk ->
      tsk
    Left _ ->
      task Nothing Nothing Nothing str

findWordsStartingWith :: Pattern -> String -> Array String
findWordsStartingWith needle hay =
  filter (\w -> (indexOf needle w) == Just 0) (split (Pattern " ") hay)


priority :: Parser (Maybe String)
priority =
  map (map singleton)
    (optionMaybe (string "(" *> upperCaseChar <* string ") "))

completed :: Parser Boolean
completed =
  option false (const true <$> string "x ")

date :: Parser String
date =
  regex """\d{4}-\d{2}-\d{2}""" <* string " "
