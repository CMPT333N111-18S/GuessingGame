{-# LANGUAGE DeriveDataTypeable #-}
module Server
    ( guessServer
    ) where

import Codec.Binary.UTF8.String
import Network.BufferType
import Network.HTTP.Server (defaultConfig,
                            insertHeader,
                            respond,
                            rqBody,
                            rspBody,
                            serverWith,
                            srvLog,
                            srvPort,
                            Handler,
                            HeaderName(HdrContentLength,HdrContentEncoding),
                            Response,
                            StatusCode(OK)
                           )
import Network.HTTP.Server.Logger (stdLogger)
import System.Random (getStdGen,randomR)
import System.Environment (getArgs)
import Text.JSON.Generic

data Guess = Guess { guess :: Int } deriving (Eq,Data,Typeable,Show)

guessServer :: IO ()
guessServer = do
    args <- map read <$> getArgs
    gen <- getStdGen
    num <- return (fst $ randomR (1,10) gen :: Int)
    let port = if null args then 2018 else head args
    serverWith defaultConfig { srvLog = stdLogger, srvPort = port } $ handleGuess (Guess num)

handleGuess :: Guess -> Handler String
handleGuess n addr url req =
    if userGuess == n
    then return $ sendText OK ("You win! The number is " ++ (show $ guess userGuess) ++ "!\n")
    else return $ sendText OK ("Try again... the number is not " ++ (show $ guess userGuess) ++ "\n")
  where userGuess = decodeJSON $ rqBody req

sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/plain"
             $ (respond s :: Response String) { rspBody = txt }
  where
    txt = encodeString v

