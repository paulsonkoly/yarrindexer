module Main where

import           Lib
import           System.Environment
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO

main :: IO ()
main = do
  args    <- getArgs
  content <- TextIO.readFile $ head args
  let doc = convert content
  putStrLn $ show $ classNames doc
