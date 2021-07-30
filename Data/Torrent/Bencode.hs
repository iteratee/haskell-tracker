{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Torrent.Bencode
Description : Types and functions for handling B-encoded data.
Copyright   : (c) Kyle Butt, 2014
License     : MIT
Maintainer  : kylebutt@gmail.com
Stability   : experimental
Portability : TypeFamilies

Bencode Data type and Bencode Catamporphisms, together with a serializer and a
deserializer.
-}
module Data.Torrent.Bencode
  ( Bencode(..)
  , BencodeC(..)
  , serialize
  , deserialize
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8   as AP
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as B
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import           Data.Int
import qualified Data.Map                           as M
import           Data.Monoid

-- | ADT representing Bittorrent B-encoded data
data Bencode
  = BeString B.ByteString -- ^ String
  | BeInt Int64 -- ^ Int (Bittorrent doesn't specify a max int size,
                 -- but 64 bits should be sufficient)
  | BeList [Bencode] -- ^ A list, consisting of B-encoded values
  | BeDict (M.Map B.ByteString Bencode) -- ^ A map from strings to B-encoded
                                       -- values
  deriving (Show)

-- | A Catamorphism for B-encoded data.
-- Using this class rather than constructing Bencode allows the program to
-- avoid storing the intermediate representation and directly serialize the
-- data. Example, Instead of
-- @
-- -- BAD EXAMPLE
-- BeDict $ M.fromAssocs [(B.pack "complete", BeInt 10),
--                        (B.pack "downloaded", BeInt 125),
--                        (B.pack "incomplete", BeInt 15)]
-- @
-- You could use:
-- @
-- beDict $ beDictCataCons (B8.pack "complete") (beInt 10) $
--          beDictCataCons (B8.pack "downloaded") (beInt 125) $
--          beDictCataCons (B8.pack "incomplete") (beInt 15) beDictCataNil
-- @
-- The second example allows you to directly serialize the result without
-- constructing an intermediate data structure.
--
-- A BencodeC instance exists for Bencode to allow you to test code that
-- produces BencodeC expressions.
class BencodeC a
  -- | ListIntermediate. Used by beList
  -- A catamorphism for Bencoded data also includes a catamorphism for a list.
  where
  data Li a :: *
  -- | DictIntermediate. Used by beDict
  -- A catamorphism for Bencoded data also includes a catamorphism for a
  -- dictionary.
  data Di a :: *
  beString :: B.ByteString -> a -- ^ Consume a ByteString as B-encoded data
  beInt :: Int64 -> a -- ^ Consume an int as B-encoded data
  beListCataNil :: Li a -- ^ Nil for the list catamorphism.
  beListCataCons :: (a -> Li a -> Li a) -- ^ Cons for the list catamorphism
  beDictCataNil :: Di a -- ^ Nil for the dict catamorphism
  beDictCataCons :: (B.ByteString -> a -> Di a -> Di a)
  -- ^ Cons-like function for the dict catamorphism. Takes a key and a value.
  beList :: Li a -> a -- ^ Complete the list catamorphism and consume an Li as
                       -- B-encoded data
  beDict :: Di a -> a -- ^ Complete the dict catamorphism and consume an Di as
                       -- B-encoded data

-- | Bencode is itself a Bencode Catamorphism.
-- In fact it's universal in that there's a natural map from this catamorphism
-- to any other.
instance BencodeC Bencode where
  data Li Bencode = BeListRaw [Bencode] -- ^ Simple wrapper type.
  data Di Bencode = BeDictRaw [(B.ByteString, Bencode)]
  -- ^ This could be a map but since this is mainly for testing, this is simple.
  beString = BeString
  beInt = BeInt
  beListCataNil = BeListRaw []
  beListCataCons x (BeListRaw xs) = BeListRaw (x : xs)
  beDictCataNil = BeDictRaw []
  beDictCataCons k v (BeDictRaw kvs) = BeDictRaw ((k, v) : kvs)
  beList (BeListRaw l) = BeList l
  beDict (BeDictRaw kvs) = BeDict . M.fromList $ kvs
  -- ^ Unwrap and then build a map.

-- | Builder is a Bencode Catamorphism.
-- Directly serialize the data via Data.ByteString.Lazy.Builder
instance BencodeC Builder where
  data Li Builder = BuilderLi Builder -- ^ A wrapped builder.
  data Di Builder = BuilderDi Builder -- ^ A wrapped builder.
  beString = serialize . BeString
  beInt = serialize . BeInt
  beListCataNil = BuilderLi mempty
  beListCataCons h (BuilderLi t) = BuilderLi (h <> t)
  beDictCataNil = BuilderDi mempty
  beDictCataCons k v (BuilderDi bldr) =
    BuilderDi $ (serialize . BeString) k <> v <> bldr
  -- | beList serializes the wrapped builder surrounded by 'l' and 'e'
  beList (BuilderLi b) = char8 'l' <> b <> char8 'e'
  -- | beDict serializes the wrapped builder surrounded by 'd' and 'e'
  beDict (BuilderDi b) = char8 'd' <> b <> char8 'e'

-- | Serialize a Bencode value to a Data.ByteString.Lazy.Builder.
-- Used for testing and also pieces are used in the BencodeC instance for
-- Builder
serialize :: Bencode -> Builder
serialize (BeString str) =
  int64Dec (fromIntegral $ B.length str) <> char8 ':' <> byteString str
serialize (BeInt i) = char8 'i' <> int64Dec i <> char8 'e'
serialize (BeList l) = char8 'l' <> mconcat (map serialize l) <> char8 'e'
serialize (BeDict m) = char8 'd' <> serializeMap m <> char8 'e'
  where
    serializeMap :: M.Map B.ByteString Bencode -> Builder
    serializeMap =
      M.foldrWithKey
        (\k v bldr -> (serialize . BeString) k <> serialize v <> bldr)
        mempty

-- | Fold over alternatives in a parser.
-- Rather than construct a list and then deconstruct it, simply pass the fold
-- directly to the parser.
-- Should be equivalent to
-- @
-- foldr cons nil <$> many v
-- @
foldAlt :: (a -> b -> b) -> b -> AP.Parser a -> AP.Parser b
foldAlt cons nil v = many_v
  where
    many_v = some_v <|> pure nil
    some_v = cons <$> v <*> many_v

-- | Fold over alternatives in a parser, with a key as well.
-- Rather than construct a list of pairs and then deconstruct it, simply pass
-- the fold directly to the parser.
-- Should be equivalent to
-- @
-- foldr (uncurry cons) nil <$> many ((,) <$> k <*> v)
-- @
foldAltKey ::
     (k -> a -> b -> b) -> b -> AP.Parser k -> AP.Parser a -> AP.Parser b
foldAltKey cons nil k v = many_kv
  where
    many_kv = some_kv <|> pure nil
    some_kv = cons <$> k <*> v <*> many_kv

-- | Deserialize is a parser that produces an arbitrary Bencode Catamorphism.
-- for testing it can produce actual Bencode
deserialize :: (BencodeC a) => AP.Parser a
deserialize =
  (beInt <$> (AP.char 'i' *> AP.decimal <* AP.char 'e')) <|>
  (beList <$> (AP.char 'l' *> listCata <* AP.char 'e')) <|>
  (beDict <$> (AP.char 'd' *> dictCata <* AP.char 'e')) <|>
  (beString <$> parseString)
  where
    listCata = foldAlt beListCataCons beListCataNil deserialize
    dictCata = foldAltKey beDictCataCons beDictCataNil parseString deserialize
    parseString = (AP.decimal <* AP.char ':') >>= AP.take
