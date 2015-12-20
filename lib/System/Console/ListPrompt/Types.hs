module System.Console.ListPrompt.Types
  where

import           Data.Default        (Default (..))
import           System.Console.ANSI

data ListPromptOptions = ListPromptOptions { selectedItemSGR :: [SGR]
                                           , normalItemSGR   :: [SGR]
                                           , mputChoice      :: Maybe (PutChoiceOptions -> IO ())
                                           }

instance Default ListPromptOptions where
    def = ListPromptOptions { normalItemSGR = [ SetColor Foreground Vivid White
                                              , SetColor Background Vivid Blue
                                              ]
                            , selectedItemSGR = [ SetColor Foreground Vivid White
                                                , SetColor Background Vivid Black
                                                ]
                            , mputChoice = Nothing
                            }

data PutChoiceOptions = PutChoiceOptions { putChoiceStr     :: String
                                         , putChoiceSuffix  :: String
                                         , putChoiceItemSgr :: [SGR]
                                         }

data ListPromptDimensions =
    ListPromptDimensions { targetCoordinate :: (Int, Int)
                         , listPromptSize   :: (Int, Int)
                         }
  deriving(Show, Ord, Eq)

instance Default ListPromptDimensions where
    def = ListPromptDimensions { targetCoordinate = (1, 2)
                               , listPromptSize = (80, 80)
                               }

type Choices = [String]
