module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStr (args !! 0 ++ " added to " ++ args !! 1 ++ " equals: ")
--    print ((read (args !! 0)) + (read (args !! 1)))
    print $ (read $ args !! 0) + (read $ args !! 1)
