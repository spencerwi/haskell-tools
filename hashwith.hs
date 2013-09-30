#!/usr/bin/env runhaskell
import Data.Char
import System.Environment
import System.Exit
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.Whirlpool as Whirlpool
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BS64

doHash :: String -> String -> BS.ByteString
doHash "md5" = BS64.encode . MD5.hash . BS.pack
doHash "sha256" = BS64.encode . SHA256.hash . BS.pack
doHash "whirlpool" = BS64.encode . Whirlpool.hash . BS.pack
doHash x = error "unrecognized hash function"

formatOutput :: String -> BS.ByteString -> String
formatOutput raw hashed = raw ++ ": " ++ (show hashed)

main = do 
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: hashwith [md5|sha256|whirlpool] STRINGS..."
            putStrLn "Hashes given strings and Base64-encodes the output"
            exitFailure

        funct:strings -> do
            let hashFunct = map toLower funct
            let hashed = map (doHash hashFunct) strings
            mapM_ putStrLn (zipWith formatOutput strings hashed)
