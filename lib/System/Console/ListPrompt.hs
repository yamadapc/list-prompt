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

import System.Console.ListPrompt.Internal
import System.Console.ListPrompt.Types

import System.Exit

simpleListPrompt :: ListPromptOptions -> Choices -> IO String
simpleListPrompt options choices = do
    dimensions <- getDimensionsIO numChoices
    waitForSelection options dimensions choices 0
  where
    numChoices = length choices

    waitForSelection options dimensions choices currentIdx = do
        clearScreen
        let st = undefined
        renderListOptions options dimensions choices
        i <- getChar
        case i of
           '\n' -> return $ choices !! currentIdx
           'j' -> waitForSelection
                      options
                      dimensions
                      choices
                      ((currentIdx + 1) `rem` numChoices)
           'k' -> waitForSelection
                      options
                      dimensions
                      choices
                      ((currentIdx - 1) `rem` numChoices)
           _ -> waitForSelection options dimensions choices currentIdx

renderListOptions :: ListPromptOptions -> ListPromptDimensions -> Choices -> IO ()
renderListOptions options dimensions choices = do
    clearScreen
    putStrLn $ "options: " ++ show options
    putStrLn $ "dimensions: " ++ show dimensions
    exitSuccess
    forM_ [0..(fst $ listPromptSize dimensions)] $ \i -> do
        drawLine options dimensions i
        cursorDown 1

    -- drawBox
    -- drawOptions

drawLine :: ListPromptOptions -> ListPromptDimensions -> Int -> IO ()
drawLine ListPromptOptions{..} ListPromptDimensions{..} n = do
    setSGR [ SetColor Foreground Vivid foregroundColor
           , SetColor Background Vivid backgroundColor
           ]

    let (y1, x1) = targetCoordinate
    setCursorPosition (y1 + n) x1

    let (_, w) = listPromptSize
    putStrLn (replicate w ' ')
