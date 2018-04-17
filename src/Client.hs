{-# LANGUAGE DeriveDataTypeable #-}
module Client
    ( guessClient
    ) where

import Data.List (intercalate)
import Network.BufferType
import Network.HTTP
import Network.URI
import System.IO
import Text.JSON.Generic

data Guess = Guess { guess :: Int } deriving (Eq,Data,Typeable,Show)

guessClient :: [String] -> IO ()
guessClient args = clientLoop serverURI
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://localhost:2018"
                       uri -> parseURI ("http://" ++ uri)

clientLoop :: URI -> IO ()
clientLoop uri = do
    g   <- promptGuess
    msg <- submitGuess g uri
    putStrLn msg
    clientLoop uri

promptGuess :: IO Guess
promptGuess = putStr "Guess a number: " >> hFlush stdout >> Guess <$> readLn

submitGuess :: Guess -> URI -> IO String
submitGuess g uri = do
    let rq = setRequestBody (mkRequest POST uri)
                            ("text/json",encodeJSON g)
    rsp <- Network.HTTP.simpleHTTP rq
    getResponseBody rsp

