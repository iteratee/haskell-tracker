{-# LANGUAGE TypeFamilies #-}
module Data.Torrent.Bencode where

import Control.Applicative
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Int
import Data.Monoid
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

data Bencode =
  BeString B.ByteString |
  BeInt Int64 |
  BeList [Bencode] |
  BeDict (M.Map B.ByteString Bencode)
  deriving (Show)

class BencodeC a where
  data Li :: * -> * -- ListIntermediate
  data Mi :: * -> * -- MapIntermediate
  beString :: B.ByteString -> a
  beInt :: Int64 -> a
  beListCataNil :: Li a
  beListCataCons :: (a -> Li a -> Li a)
  beDictCataNil :: Mi a
  beDictCataCons :: (B.ByteString -> a -> Mi a -> Mi a)
  beList :: Li a -> a
  beDict :: Mi a -> a

instance BencodeC Bencode where
  data Li Bencode = BeListRaw [Bencode]
  data Mi Bencode = BeDictRaw [(B.ByteString, Bencode)]
  beString = BeString
  beInt = BeInt
  beListCataNil = BeListRaw []
  beListCataCons x (BeListRaw xs) = BeListRaw (x:xs)
  beDictCataNil = BeDictRaw []
  beDictCataCons k v (BeDictRaw kvs) = BeDictRaw ((k,v):kvs)
  beList (BeListRaw l) = BeList l
  beDict (BeDictRaw kvs) = BeDict . M.fromList $ kvs

instance BencodeC Builder where
  data Li Builder = BuilderLi Builder
  data Mi Builder = BuilderMi Builder
  beString = serialize . BeString
  beInt = serialize . BeInt
  beListCataNil = BuilderLi mempty
  beListCataCons h (BuilderLi t) = BuilderLi (h <> t)
  beDictCataNil = BuilderMi mempty
  beDictCataCons k v (BuilderMi bldr) = BuilderMi $
    (serialize . BeString) k <> v <> bldr
  beList (BuilderLi b) = char8 'l' <> b <> char8 'e'
  beDict (BuilderMi b) = char8 'd' <> b <> char8 'e'

serialize :: Bencode -> Builder
serialize (BeString str) =
  int64Dec (fromIntegral $ B.length str) <> char8 ':' <> byteString str
serialize (BeInt i) =
  char8 'i' <> int64Dec i <> char8 'e'
serialize (BeList l) =
  char8 'l' <> mconcat (map serialize l) <> char8 'e'
serialize (BeDict m) =
  char8 'd' <> serializeMap m <> char8 'e'
  where
    serializeMap :: M.Map B.ByteString Bencode -> Builder
    serializeMap m =
      M.foldrWithKey (\k v bldr ->
        (serialize . BeString) k <> serialize v <> bldr) mempty m

foldAlt :: (a -> b -> b) -> b -> AP.Parser a -> AP.Parser b
foldAlt cons nil v = many_v
  where
    many_v = some_v <|> pure nil
    some_v = cons <$> v <*> many_v 

foldAltKey :: (k -> a -> b -> b) -> b -> AP.Parser k -> AP.Parser a -> AP.Parser b
foldAltKey cons nil k v = many_kv
  where
    many_kv = some_kv <|> pure nil
    some_kv = cons <$> k <*> v <*> many_kv 

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
    -- pair = (,) <$> bestring <*> deserialize
