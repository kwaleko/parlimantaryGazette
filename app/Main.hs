module Main where

import Control.Monad.Except 
import Core.Session 
import Effects



main :: IO ()
main = do
  val <- runAppM "/Users/lambda/dev/input2.JSON" $ runExceptT getInput
  case val of
    Right x ->runAppM "/Users/lambda/dev/output.JSON" $ runExceptT $  writeOutput (compute x)
    Left err -> runAppM "/Users/lambda/dev/output.JSON" $ runExceptT $  writeOutput err
  return ()
