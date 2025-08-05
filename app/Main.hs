{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import qualified Data.ByteString as BS
import Network.Simple.TCP (HostPreference (HostAny), closeSock, recv, send, serve)
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  hPutStrLn stderr "Logs from your program will appear here"

  -- Uncomment this block to pass stage 1
  let port = "6379"
  putStrLn $ "Redis server listening on port " ++ port
  serve HostAny port $ \(socket, address) -> do
    putStrLn $ "successfully connected client: " ++ show address
    maybeMsg <- recv socket 4096
    case maybeMsg of
      Nothing -> putStrLn "Nothing to see here"
      Just msg
        | BS.null msg -> print "Empty message"
        | BS.length msg > 100 -> print "Large message"
        | msg == "*1\r\n$4\r\nPING\r\n" -> do
            print "+PONG\r\n"
            send socket "+PONG\r\n"
        | otherwise -> print "Received normal message"

    closeSock socket
