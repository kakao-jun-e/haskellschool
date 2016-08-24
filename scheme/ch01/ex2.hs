module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let a = read(args !! 0)::Int
      b = read(args !! 1)::Int
  putStrLn(show(a) ++ " + " ++ show(b) ++ " = " ++ show(a+b))
