{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Text.Short
  ( Maybe(..)
  , just
  , nothing

  , maybe

  , isJust
  , isNothing
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe

  , toBaseMaybe
  , fromBaseMaybe
  ) where 

import Prelude hiding (Maybe,maybe)

import GHC.Base (build)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Text.Short (ShortText,toShortByteString)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)
import GHC.Exts (ByteArray#)

import GHC.Read (Read(readPrec))
import Text.Read (parens, Lexeme(Ident), lexP, (+++))
import Text.ParserCombinators.ReadPrec (prec, step)

import qualified Prelude as P

-- | Either a 'ShortText' or nothing. Do not use the
-- data constructor directly.
data Maybe = Maybe (# (# #) | ByteArray# #)

unboxShortText :: ShortText -> ByteArray#
unboxShortText x = case toShortByteString x of SBS y -> y

boxShortText :: ByteArray# -> ShortText
boxShortText x = fromShortByteStringUnsafe (SBS x)

instance Eq Maybe where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
    
instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | i #) -> showParen (p > 10)
      $ showString "just "
      . showsPrec 11 (boxShortText i)

instance Read Maybe where
  readPrec = parens $ nothingP +++ justP
    where
      nothingP = prec 10 $ do
        Ident "nothing" <- lexP
        return nothing
      justP = prec 10 $ do
        Ident "just" <- lexP
        a <- step readPrec
        return (just a)

listToMaybe :: [ShortText] -> Maybe
listToMaybe [] = nothing
listToMaybe (x:_) = just x

maybeToList :: Maybe -> [ShortText]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [ShortText]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [ShortText]
mapMaybe _ [] = []
mapMaybe f (a : as) =
  let ws = mapMaybe f as
  in maybe ws (: ws) (f a)
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (ShortText -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: ShortText -> Maybe
just x = Maybe (# | unboxShortText x #)

fromMaybe :: ShortText -> Maybe -> ShortText
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> boxShortText i

maybe :: a -> (ShortText -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> f (boxShortText i)

toBaseMaybe :: Maybe -> P.Maybe ShortText
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe ShortText -> Maybe
fromBaseMaybe = P.maybe nothing just

