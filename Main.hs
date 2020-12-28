-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library to create experimental music
-- from a mono audio and a Ukrainian text.

module Main where

import DobutokO.Sound.Executable (dobutokO2)


{-- Main function.
--}
main :: IO ()
main = dobutokO2
