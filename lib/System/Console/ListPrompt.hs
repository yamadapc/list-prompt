{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module System.Console.ListPrompt
    (
      Color
    , ListPromptOptions(..)
    , simpleListPrompt
    , PutChoiceOptions(..)
    -- Reexports
    , Default(..)
    )
  where

import           Control.Concurrent.STM
import           Control.Monad                      (forM_, when)
import           Data.Default                       (Default (..), def)
import           Graphics.Vty                       (Event (..), Key (..),
                                                     Modifier (..))
import qualified Graphics.Vty                       as Vty
import           System.Console.ANSI
import           System.IO                          (BufferMode (..), stdin)

-- Internal imports
import           System.Console.ListPrompt.Internal
import           System.Console.ListPrompt.Types

simpleListPrompt :: ListPromptOptions -> Choices -> IO (Maybe String)
simpleListPrompt options choices = setup $ do
    inp <- Vty.inputForConfig =<< Vty.standardIOConfig
    selection <- waitForSelection (Vty._eventChannel inp) 0
    setSGR []
    clearScreen
    setCursorPosition 0 0
    Vty.shutdownInput inp
    return selection
  where
    setup = withNoBuffering stdin NoBuffering . withNoCursor . withNoEcho
    numChoices = length choices

    waitForSelection ichan currentIdx = do
        clearScreen
        renderListOptions options def choices currentIdx
        e <- atomically $ readTChan ichan
        case e of
            EvKey KEnter _ -> return $ Just (choices !! currentIdx)
            EvKey (KChar 'n') [MCtrl] -> onDown
            EvKey (KChar 'j') _ -> onDown
            EvKey KDown _ -> onDown
            EvKey (KChar 'p') [MCtrl] -> onUp
            EvKey (KChar 'k') _ -> onUp
            EvKey KUp _ -> onUp
            EvKey (KChar 'q') _ -> return Nothing
            EvKey KEsc _ -> return Nothing
            _ -> waitForSelection ichan currentIdx
      where
        onDown = waitForSelection ichan ((currentIdx + 1) `rem` numChoices)
        onUp = let currentIdx' = if currentIdx == 0
                                   then length choices - 1
                                   else currentIdx - 1
                 in waitForSelection ichan currentIdx'

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
drawLine options dimensions@ListPromptDimensions{..} n =
    drawTextLine options dimensions n "" False

-- |
-- Draws a string on a list line
drawTextLine :: ListPromptOptions -> ListPromptDimensions
             -> Int    -- ^ The list item we are currently on
             -> String -- ^ The text to print
             -> Bool   -- ^ Is the currently selected item
             -> IO ()
drawTextLine opts@ListPromptOptions{..} ListPromptDimensions{..} n str selected = do
    let (y1, x1) = targetCoordinate
    setCursorPosition (y1 + n) x1

    let (_, w) = listPromptSize
    setSGR normalItemSGR
    putStr "  "
    case mputChoice of
        Just putChoice ->
            putChoice PutChoiceOptions { putChoiceStr = str
                                       , putChoiceSuffix = replicate (w - length str - 2) ' '
                                       , putChoiceItemSgr = if selected
                                                            then selectedItemSGR
                                                            else normalItemSGR
                                       }
        Nothing -> do
            when selected (setSGR selectedItemSGR)
            putStr $ str ++ replicate (w - length str - 2) ' '
            setSGR normalItemSGR
    putStrLn "  "
    setSGR [Reset]
