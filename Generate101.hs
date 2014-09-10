module Main where

import OneOhOne
import OneOhOneTalks

main = do
  generateRSS talks "msp101.rss"
  generateICS talks "msp101.ics"
  generateHTML talks "msp101.html"
