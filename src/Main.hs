module Main (main) where

import Commands (execute)
import System.Environment
import Types (mkArguments)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "The arguments are:"
  case mkArguments args of
    Left s -> print s
    Right a -> execute a
  putStrLn "--"
