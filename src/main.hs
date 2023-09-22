module Main
    ( main
    ) where

import Data.Aeson ( decode )
import Data.List (isSuffixOf)
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
      if ".rinha" `isSuffixOf` filename
        then do
          rinhaJson <- readProcess "./lib/bin/rinha" [filename] ""
          case decode (B.pack rinhaJson) of
            Just ast -> interpret ast >> return ()
            Nothing -> putStrLn "Failed to parse Rinha"
        else do
          json <- B.readFile filename
          case decode json of
            Just ast -> interpret ast >> return ()
            Nothing -> putStrLn "Failed to parse JSON"
    _ -> putStrLn "Usage: program <filename>"