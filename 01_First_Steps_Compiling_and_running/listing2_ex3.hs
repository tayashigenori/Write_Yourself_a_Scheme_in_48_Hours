module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn ("What's your name?")
    line <- getLine
    putStrLn ("Hello, " ++ line)
