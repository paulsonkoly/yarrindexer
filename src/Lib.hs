module Lib
  ( Doc
  , extractSection
  , convert
  , classes
  , methods
  )
where

import           Prelude                 hiding ( div
                                                , span
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Text.HTML.TagSoup              ( Tag(..)
                                                , (~==)
                                                , (~/=)
                                                )
import qualified Text.HTML.TagSoup             as TagSoup

import           YarrIndexer.Types

type Doc = [Tag Text]

-- | extracts a tag sub tree from a flattened tag list
--
-- the returned list is a sublist of the original, start is true on the head,
-- end is true on the last, and the elements where start is true inside have
-- matching pairs of ends, and it's the shortest such sublist. In essence if
-- tags are correctly embedded in each other, this finds the closing pair of the
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


extract :: (a -> Bool) -> (a -> Bool) -> [a] -> [[a]]
extract start end list =
  let list' = dropWhile (not . start) list
      first = extractSection start end list'
  in  case first of
        []     -> []
        first' -> first' : extract start end (drop (length first) list')


div :: [Tag Text] -> [Tag Text]
div = extractSection (~== "<div>") (~== "</div>")

span :: [Tag Text] -> [Tag Text]
span = extractSection (~== "<span>") (~== "</span>")

a :: [Tag Text] -> [Tag Text]
a = extractSection (~== "<a>") (~== "</a>")

convert :: Text -> Doc
convert = TagSoup.parseTags


classes :: Doc -> [Class]
classes doc =
  let classIndex = dropWhile (~/= "<div id=class-index>") doc
      entries    = div $ dropWhile (~/= "<div class=entries>") classIndex
      paragraphs = extract (~== "<p>") (~== "</p>") entries
  in  map processParagraph paragraphs
 where
  processParagraph p =
    let spn       = span p
        anchor    = a p
        classType = case Text.unpack $ TagSoup.innerText spn of
          "C"  -> C
          "M"  -> M
          oops -> error $ "Couldn't parse class type : " ++ oops
        lnk      = TagSoup.fromAttrib (Text.pack "href") $ head anchor
        linkText = TagSoup.innerText anchor
    in  Class classType lnk linkText


methods :: Doc -> [Method]
methods doc =
  let methodIndex = dropWhile (~/= "<div id=method-index>") doc
      entries     = div $ dropWhile (~/= "<div class=entries>") methodIndex
      paragraphs  = extract (~== "<p>") (~== "</p>") entries
  in  map processParagraph paragraphs
 where
  processParagraph p =
    let anchor   = a p
        lnk      = TagSoup.fromAttrib (Text.pack "href") $ head anchor
        linkText = TagSoup.innerText anchor
    in  Method lnk linkText
