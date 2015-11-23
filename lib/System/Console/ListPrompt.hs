{-# LANGUAGE LambdaCase      #-}
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

import           Control.Monad                      (forM_)
import           Data.Default                       (Default (..), def)
import           System.Console.ANSI
import           System.IO                          (BufferMode (..), stdin)

-- Internal imports
import           System.Console.ListPrompt.Internal
import           System.Console.ListPrompt.Types

simpleListPrompt :: ListPromptOptions -> Choices -> IO String
simpleListPrompt options choices = setup $ do
    dimensions <- getDimensionsIO numChoices
    selection <- waitForSelection dimensions 0
    setSGR []
    clearScreen
    setCursorPosition 0 0
    return selection
  where
    setup = withNoBuffering stdin NoBuffering . withNoCursor . withNoEcho
    numChoices = length choices

    waitForSelection dimensions currentIdx = do
        clearScreen
        renderListOptions options dimensions choices currentIdx
        i <- getChar
        case i of
           '\n' -> return $ choices !! currentIdx
           'j' -> waitForSelection
                      dimensions
                      ((currentIdx + 1) `rem` numChoices)
           'k' -> waitForSelection
                      dimensions
                      currentIdx'
             where
               currentIdx' = if currentIdx == 0
                                 then length choices - 1
                                 else currentIdx - 1
           _ -> waitForSelection dimensions currentIdx

renderListOptions :: ListPromptOptions
                  -> ListPromptDimensions
                  -> Choices
                  -> Int -- ^ The current selected item's index
                  -> IO ()
renderListOptions options dimensions choices currentIdx = do
    clearScreen
    forM_ [0..2] $ drawLine options dimensions

    forM_ (zip [2..] choices) $ \(i, t) ->
        drawTextLine options dimensions i t (i-2 == currentIdx)

    forM_ [len + 2..len + 3] $ drawLine options dimensions
  where
    len = length choices

-- |
-- Draws an empty list line on a said position
drawLine :: ListPromptOptions -> ListPromptDimensions -> Int -> IO ()
drawLine options dimensions@ListPromptDimensions{..} n = do
    let (_, w) = listPromptSize
    drawTextLine options dimensions n (replicate w ' ') False

-- |
-- Draws a string on a list line
drawTextLine :: ListPromptOptions -> ListPromptDimensions
             -> Int    -- ^ The list item we are currently on
             -> String -- ^ The text to print
             -> Bool   -- ^ Is the currently selected item
             -> IO ()
drawTextLine ListPromptOptions{..} ListPromptDimensions{..} n str selected = do
    setSGR $ if selected then selectedItemSGR else normalItemSGR
    let (y1, x1) = targetCoordinate
    setCursorPosition (y1 + n) x1

    let (_, w) = listPromptSize
    putStrLn $ " " ++ str ++ replicate (w - length str) ' '
