module Main where

import App
import Generador
import TinyApp.Repl

main :: IO ()
main = do
  g <- genNormal
  runRepl (app g)
