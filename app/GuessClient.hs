{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Client
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= guessClient

