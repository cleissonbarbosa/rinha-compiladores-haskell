module Main
    ( main
    ) where

import Data.Aeson ( decode )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment ( getArgs )
import Parse ()
import Eval ( interpret )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      json <- B.readFile filename
      case decode json of
        Just ast -> interpret ast >> return ()
        Nothing -> putStrLn "Failed to parse JSON"
    _ -> putStrLn "Usage: program <filename>"
