module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Data.Text as T
import qualified Test.QuickCheck
import Control.Applicative ((<$>))
import Data.Map (Map, fromList)
import Data.ByteString.Lazy
import Data.Transit
import Data.Transit.JSON

main :: IO ()
main = defaultMain tests

{-
instance Arbitrary T.Text where
  arbitrary
-}

instance (Ord k, Test.QuickCheck.Arbitrary k, Test.QuickCheck.Arbitrary v) => Test.QuickCheck.Arbitrary (Map k v) where
  arbitrary = fromList <$> Test.QuickCheck.arbitrary

instance Test.QuickCheck.Arbitrary T.Text where
  arbitrary = T.pack <$> genString
    where genChar = Test.QuickCheck.elements ['a' .. 'z']
          genString = Test.QuickCheck.listOf1 genChar

tests :: [Test]
tests = [
  testGroup "Encoding's reversibility"
    [ testProperty "Bools" (propRev :: [Bool] -> Bool)
    , testProperty "Nested bools" (propRev :: [[Bool]] -> Bool)
    , testProperty "Strings" (propRev :: [T.Text] -> Bool)
    , testProperty "Nested strings" (propRev :: [[T.Text]] -> Bool)
    , testProperty "Ints" (propRev :: [Int] -> Bool)
    , testProperty "Nested Ints" (propRev :: [[Int]] -> Bool)
    , testProperty "Maps of Int -> Int" (propRev :: [Map Int Int] -> Bool)
    , testProperty "Maps of Int -> Bool" (propRev :: [Map Int Bool] -> Bool)
    , testProperty "Maps of Bool -> Int" (propRev :: [Map Bool Int] -> Bool)
    , testProperty "Maps of String -> Maps of Int -> Int" (propRev :: [Map T.Text (Map Int Int)] -> Bool)
    ]
  ]

propRev x = decode JSON (encode JSON x :: ByteString) == Just x
