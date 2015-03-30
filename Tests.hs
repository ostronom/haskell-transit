module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Data.Text as T
import Data.ByteString.Lazy
import Data.Transit
import Data.Transit.JSON

main :: IO ()
main = defaultMain tests

{-
instance Arbitrary T.Text where
  arbitrary
-}

tests :: [Test]
tests = [
  testGroup "Encoding's reversibility"
    [ testProperty "Bools" (propRev :: [Bool] -> Bool)
    , testProperty "Nested" (propRev :: [[Bool]] -> Bool)
    , testProperty "Ints" (propRev :: [Int] -> Bool)
    ]
  ]

propRev x = decode JSON (encode JSON x :: ByteString) == Just x
