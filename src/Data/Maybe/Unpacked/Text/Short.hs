{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Text.Short
  ( MaybeShortText(..)
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
-- data constructor directly since it allows you to
-- circumvent encoding invariants.
data MaybeShortText = MaybeShortText (# (# #) | ByteArray# #)

unboxShortText :: ShortText -> ByteArray#
unboxShortText x = case toShortByteString x of SBS y -> y

boxShortText :: ByteArray# -> ShortText
boxShortText x = fromShortByteStringUnsafe (SBS x)

instance Eq MaybeShortText where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
    
instance Ord MaybeShortText where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  

instance Show MaybeShortText where
  showsPrec p (MaybeShortText m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | i #) -> showParen (p > 10)
      $ showString "just "
      . showsPrec 11 (boxShortText i)

instance Read MaybeShortText where
  readPrec = parens $ nothingP +++ justP
    where
      nothingP = prec 10 $ do
        Ident "nothing" <- lexP
        return nothing
      justP = prec 10 $ do
        Ident "just" <- lexP
        a <- step readPrec
        return (just a)

listToMaybe :: [ShortText] -> MaybeShortText
listToMaybe [] = nothing
listToMaybe (x:_) = just x

maybeToList :: MaybeShortText -> [ShortText]
maybeToList = maybe [] (: [])

catMaybes :: [MaybeShortText] -> [ShortText]
catMaybes = mapMaybe id

mapMaybe :: (a -> MaybeShortText) -> [a] -> [ShortText]
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
mapMaybeFB :: (ShortText -> r -> r) -> (a -> MaybeShortText) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: MaybeShortText -> Bool
isNothing = maybe True (const False)

isJust :: MaybeShortText -> Bool
isJust = maybe False (const True)

nothing :: MaybeShortText
nothing = MaybeShortText (# (# #) | #)

just :: ShortText -> MaybeShortText
just x = MaybeShortText (# | unboxShortText x #)

fromMaybe :: ShortText -> MaybeShortText -> ShortText
fromMaybe a (MaybeShortText m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> boxShortText i

maybe :: a -> (ShortText -> a) -> MaybeShortText -> a
maybe a f (MaybeShortText m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> f (boxShortText i)

toBaseMaybe :: MaybeShortText -> P.Maybe ShortText
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe ShortText -> MaybeShortText
fromBaseMaybe = P.maybe nothing just

