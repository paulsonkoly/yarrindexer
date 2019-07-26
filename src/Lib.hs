{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( Doc
  , extractSection
  , convert
  , classNames
  )
where

import           Data.Text                      ( Text )
import           Text.HTML.TagSoup              ( Tag(..) )
import qualified Text.HTML.TagSoup             as TagSoup
import qualified Text.HTML.TagSoup.Match       as Match


type Doc = [Tag Text]

-- | extracts a tag sub tree from a flattened tag list
-- the returned list is a sublist of the original, start is true on the head,
-- end is true on the last, and the elements where start is true inside have
-- matching pairs of ends, and it's the shortest such sublist. In essence if
-- tags are correctly embeded in each other, this finds the closing pair of the
-- first start and extract the sublist between the two.
extractSection :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
extractSection start end list =
  let list' = dropWhile (not . start) list
  in  case list' of
        []       -> []
        (x : xs) -> x : go xs (1 :: Int)
 where
  go [] _ = [] -- tag wasn't closed properly
  go _  0 = [] -- terminate
  go (x : xs) n | start x   = x : go xs (n + 1)
                | end x     = x : go xs (n - 1)
                | otherwise = x : go xs n


convert :: Text -> Doc
convert = TagSoup.parseTags


classNames :: Doc -> [Text]
classNames doc =
  let classIndex = dropWhile notClassIndex doc
      entries    = dropWhile notEntries classIndex
  in  [TagSoup.innerText $ extractDiv entries]
 where
  extractDiv = extractSection isDivOpen isDivClose
  notClassIndex =
    not . Match.tagOpen (== "div") (Match.anyAttr (== ("id", "class-index")))
  notEntries =
    not . Match.tagOpen (== "div") (Match.anyAttr (== ("class", "entries")))
  isDivOpen  = Match.tagOpen (== "div") (const True)
  isDivClose = Match.tagClose (== "div")
