{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Transit.Internal where

import Data.Text (Text)
import Data.Map (Map)
import Control.Applicative ((<$>))

data Value = Bool Bool
           | String Text
           | Array [Value]
           | Dict [(Value, Value)]
           | Null
           deriving Show

class ToTransit a where
  toTransit :: a -> Value

class FromTransit a where
  fromTransit :: Value -> Maybe a

class Repr a s where
  encode :: a -> Value -> s
  decode :: a -> s -> Maybe Value

--- Impls

instance ToTransit Bool where
  toTransit = Bool

instance ToTransit Text where
  toTransit = String

instance ToTransit a => ToTransit (Maybe a) where
  toTransit (Just v) = toTransit v
  toTransit Nothing  = Null

--instance (ToTransit k, ToTransit v)=> ToTransit (Map k v) where
--  toTransit = Dict . map (\(k, v) -> (toTransit k, toTransit v)) . toList

instance ToTransit a => ToTransit [a] where
  toTransit = Array . fmap toTransit
