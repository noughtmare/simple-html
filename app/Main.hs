{-# LANGUAGE OverloadedStrings #-}

import Network.Run.TCP (runTCPServer)
import qualified Network.Socket.ByteString.Lazy as Net
import qualified Data.ByteString.Lazy.Char8 as Str
import Numeric (readHex)
import Data.Char (chr)
import Debug.Trace
import Lib (sanitize)

main = do
  putStrLn "Server started at 127.0.0.1:9999"
  runTCPServer (Just "127.0.0.1") "9999" $ \s -> do
    request <- Net.getContents s
    case Str.words (Str.takeWhile (/= '\r') request) of
      ["GET", resource, "HTTP/1.1"] -> do
        putStrLn ("Received request for: " ++ Str.unpack resource)
        let res = Str.pack $ sanitize $ Str.unpack $ urlDecode $ Str.dropWhile (== '/') resource
        putStrLn $ "Sending response: " <> Str.unpack res
        Net.sendAll s ("HTTP/1.1 200 OK\r\n\r\n" <> res <> "\r\n")
      _ -> error "todo"

urlDecode :: Str.ByteString -> Str.ByteString
urlDecode xs =
  case Str.split '%' xs of
    [] -> ""
    (x:xs) -> Str.concat (x : map f xs)
  where
    f xs
      | Str.length xs >= 2 =
        let (num, rest) = Str.splitAt 2 xs in
        Str.cons (chr $ fst $ head $ readHex $ Str.unpack num) rest
      | otherwise = xs
