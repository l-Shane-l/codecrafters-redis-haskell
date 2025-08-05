{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Void
import Network.Simple.TCP (HostPreference (HostAny), Socket, closeSock, recv, send, serve)
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

-- Parse RESP protocol
data RESP
  = Array Int [RESP]
  | BulkString ByteString
  deriving (Show, Eq)

-- Simple RESP parser
resp :: Parser RESP
resp = array <|> bulkString
 where
  array = do
    char 42
    n <- L.decimal
    string "\r\n"
    elements <- count n resp
    return (Array n elements)

  bulkString = do
    char 36
    len <- L.decimal
    string "\r\n"
    str <- takeP Nothing len
    string "\r\n"
    return (BulkString str)

-- Parse and handle commands
handleClient :: Socket -> IO ()
handleClient socket = loop BS.empty
 where
  loop buffer = do
    maybeData <- recv socket 4096
    case maybeData of
      Nothing -> return ()
      Just newData -> processInput (buffer <> newData)

  processInput input =
    case runParser resp "" input of
      Left _ -> loop input -- Need more data
      Right (Array 1 [BulkString "PING"]) -> do
        send socket "+PONG\r\n"
        -- Find how much we consumed and process the rest
        let consumed = BS.length "*1\r\n$4\r\nPING\r\n"
        processInput (BS.drop consumed input)
      Right _ -> do
        send socket "-ERR unknown command\r\n"
        closeSock socket

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  hPutStrLn stderr "Logs from your program will appear here"

  let port = "6379"
  putStrLn $ "Redis server listening on port " ++ port

  serve HostAny port $ \(socket, address) -> do
    putStrLn $ "successfully connected client: " ++ show address
    handleClient socket
    closeSock socket

