module System.Console.ListPrompt.Types
  where

import Data.Default (Default(..))
import System.Console.ANSI (Color(..))

data ListPromptOptions = ListPromptOptions { backgroundColor :: Color
                                           , foregroundColor :: Color
                                           }
  deriving(Show, Ord, Eq)

instance Default ListPromptOptions where
    def = ListPromptOptions { backgroundColor = Blue
                            , foregroundColor = White
                            }

data ListPromptDimensions =
    ListPromptDimensions { targetCoordinate :: (Int, Int)
                         , listPromptSize :: (Int, Int)
                         }
  deriving(Show, Ord, Eq)

instance Default ListPromptDimensions where
    def = ListPromptDimensions { targetCoordinate = (0, 0)
                               , listPromptSize = (80, 80)
                               }

type Choices = [String]
