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

tests :: [Test]
tests = [
  testGroup "Encoding's reversibility"
    [ testProperty "Bools" (propRev :: [Bool] -> Bool)
    , testProperty "Nested" (propRev :: [[Bool]] -> Bool)
    , testProperty "Ints" (propRev :: [Int] -> Bool)
    , testProperty "Nested Ints" (propRev :: [[Int]] -> Bool)
    , testProperty "Maps of Ints" (propRev :: [Map Int Int] -> Bool)
    ]
  ]

propRev x = decode JSON (encode JSON x :: ByteString) == Just x
