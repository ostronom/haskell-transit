{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Transit.Internal where

import Data.Text (Text)
import Data.Map (Map)
import Control.Applicative ((<$>))

data Value = Bool Bool
           | String Text
           | Array [Value]
           | Int Int
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

instance ToTransit Int where
  toTransit = Int

instance ToTransit a => ToTransit (Maybe a) where
  toTransit (Just v) = toTransit v
  toTransit Nothing  = Null

--instance (ToTransit k, ToTransit v)=> ToTransit (Map k v) where
--  toTransit = Dict . map (\(k, v) -> (toTransit k, toTransit v)) . toList

instance ToTransit a => ToTransit [a] where
  toTransit = Array . fmap toTransit

instance FromTransit Bool where
  fromTransit (Bool val) = Just val
  fromTransit _ = Nothing

instance FromTransit Text where
  fromTransit (String val) = Just val
  fromTransit _ = Nothing

instance FromTransit Int where
  fromTransit (Int i) = Just i

instance FromTransit a => FromTransit (Maybe a) where
  fromTransit Null = Just Nothing
  fromTransit value = Just <$> fromTransit value

instance FromTransit a => FromTransit [a] where
  fromTransit (Array val) = sequence $ fromTransit <$> val
  fromTransit _ = Nothing

{-
instance (FromTransit k, FromTransit v) => FromTransit (Map k v) where
  fromTransit (Dict val) = fromList <$> mapM from val
    where from (k, v) = case (fromTransit k, fromTransit v) of
                          (Just k', Just v') -> Just (k', v')
                          _ -> Nothing
  fromTransit _ = Nothing
-}
