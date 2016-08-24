module Main where

main :: IO ()
main = do
  putStrLn("what's your name?")
  name <- getLine
  putStrLn("Master " ++ name ++ "!")
