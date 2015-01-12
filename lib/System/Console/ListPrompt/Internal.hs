module System.Console.ListPrompt.Internal
  where

import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import System.Console.ANSI
import System.Console.Terminal.Size (size, Window(..))
import System.IO (hGetBuffering, hSetBuffering, hSetEcho, stdin)

import System.Console.ListPrompt.Types

-- |
-- Calculates the optimal position for the prompt optionally with the
-- terminal's size and -- the number of available choices
getDimensions :: Int -> Maybe (Window Int) -> ListPromptDimensions
getDimensions numChoices (Just (Window h w)) =
    ListPromptDimensions { targetCoordinate = tc
                         , listPromptSize = lps
                         }
  where
    margin = 3
    padding = 3
    tc = ((h `div` 2 - numChoices `div` 2) - padding, padding)
    lps = (numChoices + margin, w - margin * 2)

getDimensionsIO :: Int -> IO ListPromptDimensions
getDimensionsIO n = getDimensions n <$> size

withNoCursor = bracket_ hideCursor showCursor
withNoEcho = bracket_ (hSetEcho stdin False) (hSetEcho stdin True)

withNoBuffering handle newBuffering action = do
    originalBuffering <- hGetBuffering handle
    bracket_
        (hSetBuffering handle newBuffering)
        (hSetBuffering handle originalBuffering)
        action
