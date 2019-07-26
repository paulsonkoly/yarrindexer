{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib
import           System.Environment
import           Control.Monad
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Class

main :: IO ()
main = do
  args    <- getArgs
  content <- TextIO.readFile $ head args
  let doc = convert content
  forM_ (classes doc) $ \(Class t l n) ->
    TextIO.putStrLn $ Text.intercalate "%%" [Text.pack $ show t, l, n]
