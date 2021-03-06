module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Data.Text as T
import qualified Test.QuickCheck
import Control.Applicative ((<$>))
import Control.Monad
import Data.Map (Map, fromList)
import Data.Time
import Data.Time.Clock
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

instance Test.QuickCheck.Arbitrary Day where
    arbitrary = ModifiedJulianDay `liftM` Test.QuickCheck.arbitrary

instance Test.QuickCheck.Arbitrary DiffTime where
    arbitrary = (picosecondsToDiffTime . (* 1000000000)) <$> Test.QuickCheck.choose (0, 86400000)

instance Test.QuickCheck.Arbitrary UTCTime where
  arbitrary = liftM2 UTCTime Test.QuickCheck.arbitrary Test.QuickCheck.arbitrary

instance Test.QuickCheck.Arbitrary T.Text where
  arbitrary = T.pack <$> genString
    where genChar = Test.QuickCheck.elements ['a' .. 'z']
          genString = Test.QuickCheck.listOf1 genChar

tests :: [Test]
tests = [
  testGroup "Encoding's reversibility"
    [ testProperty "Bools" (propRev :: [Bool] -> Bool)
    --, testProperty "Strings" (propRev :: [T.Text] -> Bool)
    --, testProperty "Ints" (propRev :: [Int] -> Bool)
    --, testProperty "Floats" (propRev :: [Float] -> Bool)
    , testProperty "Dates" (propRev :: [UTCTime] -> Bool)
    --, testProperty "Maps of Int -> Int" (propRev :: [Map Int Int] -> Bool)
    --, testProperty "Maps of Int -> Bool" (propRev :: [Map Int Bool] -> Bool)
    --, testProperty "Maps of Bool -> Int" (propRev :: [Map Bool Int] -> Bool)
    --, testProperty "Maps of String -> Maps of Int -> Int" (propRev :: [Map T.Text (Map Int Int)] -> Bool)
    ]
  ]

propRev x = decode JSON (encode JSON x :: ByteString) == Just x
