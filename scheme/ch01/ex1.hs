module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("First arg: " ++ (args !! 0) ++ ", Second arg: " ++ (args !! 1))
