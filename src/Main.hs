module Main
    ( main
    ) where

import Data.Aeson ( decode )
import Data.List (isSuffixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment ( getArgs )
import System.Process (readProcess)
import Parse ()
import Eval ( interpret )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      start <- getCurrentTime
      if ".rinha" `isSuffixOf` filename
        then do
          rinhaJson <- readProcess "./lib/bin/rinha.exe" [filename] ""
          case decode (B.pack rinhaJson) of
            Just ast -> interpret ast >> return ()
            Nothing -> putStrLn "Failed to parse Rinha"
        else do
          json <- B.readFile filename
          case decode json of
            Just ast -> interpret ast >> return ()
            Nothing -> putStrLn "Failed to parse JSON"
      end <- getCurrentTime
      let diff = diffUTCTime end start
      putStrLn $ "\n\nElapsed time: " ++ show diff
    _ -> putStrLn "Usage: program <filename>"
