module System.Console.ListPrompt
    (
      Color
    , ListPromptOptions(..)
    , simpleListPrompt
    -- Reexports
    , Default(..)
    )
  where

import Control.Monad (forM_)
import Data.Default (Default(..), def)
import System.Console.ANSI
import System.Console.Terminal.Size (size, Window(..))

data ListPromptOptions = ListPromptOptions { backgroundColor :: Color
                                           , foregroundColor :: Color
                                           }

data ListPromptDimensions =
    ListPromptDimensions { targetCoordinate :: (Int, Int)
                         , listPromptSize :: (Int, Int)
                         }

instance Default ListPromptOptions where
    def = ListPromptOptions { backgroundColor = Blue
                            , foregroundColor = White
                            }

simpleListPrompt :: ListPromptOptions -> [String] -> IO a
simpleListPrompt options choices = do
    clearScreen
    renderListOptions options choices
    return undefined

renderListOptions :: ListPromptOptions -> [String] -> IO ()
renderListOptions options choices = do
    mwindow <- size
    case mwindow of
       Just (Window h w) -> do
           clearScreen
           drawBox
           drawOptions
           return ()
       Nothing -> return ()
  where
    drawBox = undefined
        -- forM_ [1..h] $ do
            -- putStrLn ""

    drawOptions = undefined

drawLine :: ListPromptOptions -> ListPromptDimensions -> Color -> IO ()
drawLine _ _ = undefined -- do
