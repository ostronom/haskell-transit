{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances #-}
module Data.Transit.JSON where

import Data.Text (Text)
import Data.Scientific (base10Exponent)
import qualified Data.Vector as V
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import Data.Transit.Internal

data JSON = JSON

data AsKey = AsKey | AsVal

str :: Text -> J.Value
str = J.String

instance Repr JSON ByteString where
  encode JSON val = J.encode (AsVal, val)
  decode JSON = J.decode

instance J.ToJSON (AsKey, Value) where
  toJSON (AsVal, Bool val) = J.Bool val
  toJSON (AsVal, Null) = J.Null
  toJSON (AsKey, Null) = str "~_"
  toJSON (AsKey, Bool False) = str "~?f"
  toJSON (AsKey, Bool True) = str "~?t"
  toJSON (_, String val) = J.toJSON val
  toJSON (_, Array val) = J.Array $ V.fromList $ map (J.toJSON . (,) AsVal) val
  toJSON (_, Int val) = J.toJSON val
--  toJSON (_, Dict val) = dictToJson val

instance J.FromJSON Value where
  parseJSON (J.Bool b) = return $ Bool b
  parseJSON (J.Number b) = return $ Int (base10Exponent b) -- TODO:!!!!
  parseJSON (J.Array vec) = Array `fmap` mapM J.parseJSON (V.toList vec)
