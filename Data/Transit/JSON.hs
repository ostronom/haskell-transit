{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances #-}
module Data.Transit.JSON where

import Debug.Trace

import qualified Data.Text as T
import Data.Scientific (toBoundedInteger, fromFloatDigits, toRealFloat)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import Data.Transit.Internal

data JSON = JSON

data AsWhat = AsKey | AsVal

str :: T.Text -> J.Value
str = J.String

dictToJson :: [(Value, Value)] -> J.Value
dictToJson vals = J.Array $ V.fromList $ (str "^ " :) $ flatten $ map jsonPair vals
  where jsonPair (k, v) = (J.toJSON (AsKey, k), J.toJSON (AsVal, v))
        flatten [] = []
        flatten ((k,v) : kvs) = k : v : flatten kvs

jsonToDict jvals = do
  vals <- mapM J.parseJSON jvals
  return $ if even (length vals) then Dict (toPairs vals) else Dict []
  where toPairs :: [Value] -> [(Value, Value)]
        toPairs [] = []
        toPairs (x:y:xs) = (x,y) : toPairs xs

parseTransitArray vec =
  case vec V.!? 0 of
    -- object
    Just (J.String "^ ") -> jsonToDict $ V.toList $ V.tail vec
    _                    -> Array `fmap` mapM J.parseJSON (V.toList vec)


{- TODO: implement extensions -}

parseTaggedString' ('i':rest) = case toBoundedInteger $ read rest of
                                  Just num -> Int num
                                  Nothing  -> Null
parseTaggedString' "?f" = Bool False
parseTaggedString' "?t" = Bool True
parseTaggedString' x = Null


parseTransitString s =
  if T.head s == '~' then parseTaggedString' $ T.unpack $ T.tail s else String s

taggedString tag stringVal = str $ T.concat ["~", tag, T.pack stringVal]

instance Repr JSON ByteString where
  encode JSON val = J.encode (AsVal, val)
  decode JSON = J.decode

instance J.ToJSON (AsWhat, Value) where
  toJSON (AsVal, Bool val) = J.Bool val
  toJSON (AsVal, Null) = J.Null
  toJSON (AsKey, Null) = str "~_"
  toJSON (AsKey, Bool False) = taggedString "?" "f"
  toJSON (AsKey, Bool True) = taggedString "?" "t"
  toJSON (_, String val) = J.toJSON val
  toJSON (_, Array val) = J.Array $ V.fromList $ map (J.toJSON . (,) AsVal) val
  toJSON (AsKey, Int val) = taggedString "i" (show val)
  toJSON (AsVal, Int val) = J.Number (fromFloatDigits (fromIntegral val))
  toJSON (AsKey, Float val) = taggedString "d" (show val)
  toJSON (AsVal, Float val) = J.Number (fromFloatDigits val)
  toJSON (_, Dict val) = dictToJson val

instance J.FromJSON Value where
  parseJSON (J.Bool b) = return $ Bool b
  parseJSON (J.String s) = return $ parseTransitString s
  parseJSON (J.Number n) = return $ case toBoundedInteger n of
                                      Just v  -> Int v
                                      Nothing -> Float (toRealFloat n)
  parseJSON (J.Array vec) = parseTransitArray vec
