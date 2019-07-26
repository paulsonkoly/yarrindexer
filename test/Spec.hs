module Main
  ( main
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           Lib
import           Control.Monad


-- correctly parenthesized digits with maximum embeding depth allowed
tags :: Int -> Gen [Char]
tags 0 = listOf $ elements ['0' .. '9']
tags n = do
  elems <- listOf $ tags (n - 1)
  concat <$> forM elems parenthesize
 where
  parenthesize :: [Char] -> Gen [Char]
  parenthesize c = oneof [return c, return $ concat ["(", c, ")"]]


isOpen :: Char -> Bool
isOpen = (== '(')


isClose :: Char -> Bool
isClose = (== ')')


extractParens :: [Char] -> [Char]
extractParens = extractSection isOpen isClose


main :: IO ()
main = hspec $ do
  describe "extractSection" $ do
    context "with empty list" $ do
      it "gives back the empty list"
        $          extractSection (const True) (const True) ([] :: [Int])
        `shouldBe` ([] :: [Int])
    it "starts with a list where start is true" $ forAll (tags 3) $ \t ->
      let result = extractParens t in result === [] .||. isOpen (head result)
    it "ends with a list where end is true"
      $ forAll (tags 3)
      $ \t ->
          let result = extractParens t
          in  result === [] .||. isClose (last result)
    it "has equal number of opens and closes" $ forAll (tags 3) $ \t ->
      let count f = length . (filter f)
          result = extractParens t
      in  count isOpen result == count isClose result


