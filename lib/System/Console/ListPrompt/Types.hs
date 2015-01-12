module System.Console.ListPrompt.Types
  where

import Data.Default (Default(..))
import System.Console.ANSI

data ListPromptOptions = ListPromptOptions { selectedItemSGR :: [SGR]
                                           , normalItemSGR :: [SGR]
                                           }
  deriving(Show, Ord, Eq)

instance Default ListPromptOptions where
    def = ListPromptOptions { normalItemSGR = [ SetColor Foreground Vivid White
                                              , SetColor Background Vivid Blue
                                              ]
                            , selectedItemSGR = [ SetColor Foreground Vivid White
                                                , SetColor Background Vivid Red
                                                ]
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
