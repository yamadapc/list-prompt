import Control.Monad
import System.Console.ListPrompt (def, simpleListPrompt)

main :: IO ()
main = do
    putStrLn "Type one list item per line and then press enter twice:"
    items <- readItems
    simpleListPrompt def items

readItems :: IO [String]
readItems = loop []
  where
    loop acc = do
        ln <- getLine
        if null ln
            then return acc
            else loop (ln : acc)
