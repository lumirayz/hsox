module Main where

import Types
import Osc
import Output

main = outputSox 22000 20 osc

osc :: Osc
osc = freq (sinWF 0.1) (sinWF 1000)
