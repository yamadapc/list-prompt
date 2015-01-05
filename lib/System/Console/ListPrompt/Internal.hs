module System.Console.ListPrompt.Internal
  where

import Control.Applicative ((<$>))
import System.Console.Terminal.Size (size, Window(..))

import System.Console.ListPrompt.Types

-- |
-- Calculates the optimal position for the prompt optionally with the
-- terminal's size and -- the number of available choices
--
-- >>> listPromptSize (getDimensions 10 (Just (Window 80 80)))
-- (undefined, undefined)
getDimensions :: Int -> Maybe (Window Int) -> ListPromptDimensions
getDimensions numChoices (Just (Window h w)) =
    ListPromptDimensions (h `div` 2 - numChoices `div` 2 + padding, padding)
                         (numChoices + margin, w - margin)
  where
    padding = 3
    margin = 3

getDimensionsIO :: Int -> IO ListPromptDimensions
getDimensionsIO n = getDimensions n <$> size
