module Main where

import TwoOhOne
import TwoOhOneLiterature

main :: IO ()
main = do
  (usual,lits) <- litsFromFile
  generateHTML usual lits "msp201.html"

