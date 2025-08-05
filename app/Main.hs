{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Void
import Network.Simple.TCP (HostPreference (HostAny), Socket, accept, closeSock, listen, recv, send)
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import qualified Data.Char as BS8
import RESP

type Parser = Parsec Void ByteString

data RESPValue
  = Array Int [RESPValue]
  | BulkString ByteString
  | SimpleString ByteString
  | Error ByteString
  deriving (Show, Eq)

data Command
  = Ping
  | Echo ByteString
  | Unknown
  deriving (Show, Eq)

respValue :: Parser RESPValue
respValue = array <|> bulkString <|> simpleString <|> respError
 where
  array = do
    _ <- char arrayMarker
    n <- L.decimal
    _ <- string lineEnding
    elements <- count n respValue
    return (Array n elements)

  bulkString = do
    _ <- char bulkStringMarker
    len <- L.decimal
    _ <- string lineEnding
    str <- takeP Nothing len
    _ <- string lineEnding
    return (BulkString str)

  simpleString = do
    _ <- char simpleStringMarker
    str <- takeWhileP Nothing (/= carriageReturn)
    _ <- string lineEnding
    return (SimpleString str)

  respError = do
    _ <- char errorMarker
    str <- takeWhileP Nothing (/= carriageReturn)
    _ <- string lineEnding
    return (Error str)

parseCommand :: RESPValue -> Command
parseCommand (Array _ (BulkString cmd : args)) =
  case BS8.map BS8.toUpper cmd of
    "PING" -> Ping
    "ECHO" -> case args of
      [BulkString msg] -> Echo msg
      _ -> Unknown
    _ -> Unknown
parseCommand _ = Unknown

executeCommand :: Command -> RESPValue
executeCommand Ping = SimpleString "PONG"
executeCommand (Echo msg) = BulkString msg
executeCommand Unknown = Error "ERR unknown command"

encodeRESP :: RESPValue -> ByteString
encodeRESP (SimpleString s) =
  BS.cons simpleStringMarker (terminate s)
encodeRESP (Error e) =
  BS.cons errorMarker (terminate e)
encodeRESP (BulkString s) =
  BS.concat
    [ BS.singleton bulkStringMarker
    , BS8.pack (show (BS.length s))
    , lineEnding
    , s
    , lineEnding
    ]
encodeRESP (Array n items) =
  BS.concat
    [ BS.singleton arrayMarker
    , BS8.pack (show n)
    , lineEnding
    , mconcat (map encodeRESP items)
    ]

handleClient :: Socket -> IO ()
handleClient socket = loop BS.empty
 where
  loop buffer = do
    maybeData <- recv socket 4096
    case maybeData of
      Nothing -> return ()
      Just newData ->
        if BS.null newData
          then return ()
          else processCommands (buffer <> newData)

  processCommands input = do
    case runParser ((,) <$> respValue <*> getOffset) "" input of
      Left _ -> loop input
      Right (parsed, consumed) -> do
        let cmd = parseCommand parsed
        let response = executeCommand cmd
        send socket (encodeRESP response)

        let remaining = BS.drop consumed input
        if BS.null remaining
          then loop BS.empty
          else processCommands remaining

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hPutStrLn stderr "Logs from your program will appear here"

  let port = "6379"
  putStrLn $ "Redis server listening on port " ++ port

  listen HostAny port $ \(lsocket, _) -> forever $ do
    accept lsocket $ \(socket, address) -> do
      putStrLn $ "successfully connected client: " ++ show address
      forkIO $ do
        handleClient socket
        closeSock socket

