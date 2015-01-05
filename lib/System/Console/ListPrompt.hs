{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module System.Console.ListPrompt
    (
      Color
    , ListPromptOptions(..)
    , simpleListPrompt
    -- Reexports
    , Default(..)
    )
  where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Default (Default(..), def)
import Data.Maybe (maybe)
import System.Console.ANSI
import System.Console.Terminal.Size (size, Window(..))

data ListPromptOptions = ListPromptOptions { backgroundColor :: Color
                                           , foregroundColor :: Color
                                           }

instance Default ListPromptOptions where
    def = ListPromptOptions { backgroundColor = Blue
                            , foregroundColor = White
                            }

data ListPromptDimensions =
    ListPromptDimensions { targetCoordinate :: (Int, Int)
                         , listPromptSize :: (Int, Int)
                         }

instance Default ListPromptDimensions where
    def = ListPromptDimensions { targetCoordinate = (0, 0)
                               , listPromptSize = (80, 80)
                               }

simpleListPrompt :: ListPromptOptions -> [String] -> IO ()
simpleListPrompt options choices = do
    clearScreen
    renderListOptions options choices

renderListOptions :: ListPromptOptions -> [String] -> IO ()
renderListOptions options choices = do
    dimensions <- getDimensions
    clearScreen
    drawLine options dimensions
    -- drawBox
    -- drawOptions
  where
    drawBox = undefined
        -- forM_ [1..h] $ do
            -- putStrLn ""

    drawOptions = undefined

getDimensions :: IO ListPromptDimensions
getDimensions = maybe def windowToDimensions <$> size
  where
    windowToDimensions (Window h w) = ListPromptDimensions (0, 0) (h, w)

drawLine :: ListPromptOptions -> ListPromptDimensions -> IO ()
drawLine ListPromptOptions{..} ListPromptDimensions{..} = do
    setSGR [ SetColor Foreground Vivid foregroundColor
           , SetColor Background Vivid backgroundColor
           ]
    uncurry setCursorPosition targetCoordinate
    let (h, w) = listPromptSize
    putStrLn (replicate w ' ')
