{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib
import           Control.Monad
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Options.Applicative
import           System.IO

import           YarrIndexer.Types


data CmdLine = CmdLine
  { klassOpt :: Bool
  , input :: String
  , output :: String
  }


options :: Parser CmdLine
options =
  CmdLine
    <$> switch
          (short 'C' <> help "Generate class index (method index is default)")
    <*> strOption
          (long "input" <> short 'i' <> value "stdin" <> help "input file")
    <*> strOption
          (long "output" <> short 'o' <> value "stdout" <> help "output file")


fnToHandleR :: FilePath -> IO Handle
fnToHandleR "stdin" = return stdin
fnToHandleR fn      = openFile fn ReadMode


fnToHandleW :: FilePath -> IO Handle
fnToHandleW "stdout" = return stdout
fnToHandleW fn       = openFile fn WriteMode


main :: IO ()
main = do
  cmdLine <- execParser $ info options fullDesc
  iHandle <- fnToHandleR $ input cmdLine
  oHandle <- fnToHandleW $ output cmdLine
  content <- TextIO.hGetContents iHandle
  let doc = convert content
  if klassOpt cmdLine
    then forM_ (classes doc) $ \(Class t l n) ->
      TextIO.hPutStrLn oHandle
        $ Text.intercalate "%%" [Text.pack $ show t, l, n]
    else forM_ (methods doc) $ \(Method l n) ->
      TextIO.hPutStrLn oHandle $ Text.intercalate "%%" [l, n]
