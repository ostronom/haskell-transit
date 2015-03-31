module Data.Transit (encode, decode) where

import qualified Data.Transit.Internal as I
import Data.Transit.JSON


encode :: (I.Repr a s, I.ToTransit v) => a -> v -> s
encode repr = I.encode repr . I.toTransit

decode :: (I.Repr a s, I.FromTransit v) => a -> s -> Maybe v
decode repr str = I.decode repr str >>= I.fromTransit
