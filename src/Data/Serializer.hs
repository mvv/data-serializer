{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Serialization monoid and serializable types.
module Data.Serializer
  (
  -- * Serialization monoid
    Serializer(..)
  -- ** Binary words serialization
  , word16H
  , word32H
  , word64H
  , word
  , wordL
  , wordB
  , wordH
  , int8
  , int16
  , int16L
  , int16B
  , int16H
  , int32
  , int32L
  , int32B
  , int32H
  , int64
  , int64L
  , int64B
  , int64H
  , int
  , intL
  , intB
  , intH
  -- ** Endian serializers
  , LittleEndianSerializer(..)
  , BigEndianSerializer(..)
  , serializeIn
  , serializeH
  -- * Serializable types
  , Serializable(..)
  , putIn
  , putL
  , putB
  , putH
  , SizedSerializable(..)
  , RestSerializable(..)
  ) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Proxy (Proxy(..))
import Data.Monoid (Monoid, (<>))
import Data.Endian (Endian(..), swapEndian)
import Data.Word
import Data.Int
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.Binary.Put as B
import qualified Data.Serialize.Put as S

-- | Serialization monoid.
class Monoid s ⇒ Serializer s where
  {-# MINIMAL word8 #-}
  -- | Default byte order of the serializer.
  endian ∷ Proxy s → Endian
#ifdef WORDS_BIGENDIAN
  endian _ = BigEndian
#else
  endian _ = LittleEndian
#endif
  {-# INLINE endian #-}
  -- | Serialize a byte. 'word8' /x/ = 'byteString' ('BS.singleton' /x/)
  word8 ∷ Word8 → s
  -- | Serialize an unsigned 16-bit integer in the default byte order.
  word16 ∷ Word16 → s
  word16 = putIn (endian (Proxy ∷ Proxy s))
  {-# INLINE word16 #-}
  -- | Serialize an unsigned 32-bit integer in the default byte order.
  word32 ∷ Word32 → s
  word32 = putIn (endian (Proxy ∷ Proxy s))
  {-# INLINE word32 #-}
  -- | Serialize an unsigned 64-bit integer in the default byte order.
  word64 ∷ Word64 → s
  word64 = putIn (endian (Proxy ∷ Proxy s))
  {-# INLINE word64 #-}
  -- | Serialize an unsigned 16-bit integer in little endian.
  word16L ∷ Word16 → s
  word16L w =  word8 (fromIntegral w)
            <> word8 (fromIntegral (shiftR w 8))
  {-# INLINE word16L #-}
  -- | Serialize an unsigned 16-bit integer in big endian.
  word16B ∷ Word16 → s
  word16B = word16L . swapEndian
  {-# INLINE word16B #-}
  -- | Serialize an unsigned 32-bit integer in little endian.
  word32L ∷ Word32 → s
  word32L w =  word8 (fromIntegral w)
            <> word8 (fromIntegral (shiftR w 8))
            <> word8 (fromIntegral (shiftR w 16))
            <> word8 (fromIntegral (shiftR w 24))
  {-# INLINE word32L #-}
  -- | Serialize an unsigned 32-bit integer in big endian.
  word32B ∷ Word32 → s
  word32B = word32L . swapEndian
  {-# INLINE word32B #-}
  -- | Serialize an unsigned 64-bit integer in little endian.
  word64L ∷ Word64 → s
  word64L w =  word8 (fromIntegral w)
            <> word8 (fromIntegral (shiftR w 8))
            <> word8 (fromIntegral (shiftR w 16))
            <> word8 (fromIntegral (shiftR w 24))
            <> word8 (fromIntegral (shiftR w 32))
            <> word8 (fromIntegral (shiftR w 40))
            <> word8 (fromIntegral (shiftR w 48))
            <> word8 (fromIntegral (shiftR w 56))
  {-# INLINE word64L #-}
  -- | Serialize an unsigned 64-bit integer in big endian.
  word64B ∷ Word64 → s
  word64B = word64L . swapEndian
  {-# INLINE word64B #-}
  -- | Serialize a 'BS.ByteString'. Must be a monoid homomorphism.
  byteString ∷ BS.ByteString → s
  byteString = mconcat . fmap word8 . BS.unpack
  {-# INLINE byteString #-}
  -- | Serialize a 'SBS.ShortByteString'. Must be a monoid homomorphism.
  shortByteString ∷ SBS.ShortByteString → s
  shortByteString = mconcat . fmap word8 . SBS.unpack
  {-# INLINE shortByteString #-}
  -- | Serialize a lazy 'LBS.ByteString'. Must be a monoid homomorphism.
  lazyByteString ∷ LBS.ByteString → s
  lazyByteString = mconcat . fmap byteString . LBS.toChunks
  {-# INLINE lazyByteString #-}
  -- | Serialize a 'BB.Builder'. Must be a monoid homomorphism.
  builder ∷ BB.Builder → s
  builder = lazyByteString . BB.toLazyByteString
  {-# INLINE builder #-}

instance Serializer [Word8] where
  word8 = pure
  {-# INLINE word8 #-}

instance Serializer BB.Builder where
  word8 = BB.word8
  {-# INLINE word8 #-}
  word16L = BB.word16LE
  {-# INLINE word16L #-}
  word16B = BB.word16BE
  {-# INLINE word16B #-}
  word32L = BB.word32LE
  {-# INLINE word32L #-}
  word32B = BB.word32BE
  {-# INLINE word32B #-}
  word64L = BB.word64LE
  {-# INLINE word64L #-}
  word64B = BB.word64BE
  {-# INLINE word64B #-}
  byteString = BB.byteString
  {-# INLINE byteString #-}
  shortByteString = BB.shortByteString
  {-# INLINE shortByteString #-}
  lazyByteString = BB.lazyByteString
  {-# INLINE lazyByteString #-}
  builder = id
  {-# INLINE builder #-}

instance Serializer B.Put where
  word8 = B.putWord8
  {-# INLINE word8 #-}
  word16L = B.putWord16le
  {-# INLINE word16L #-}
  word16B = B.putWord16be
  {-# INLINE word16B #-}
  word32L = B.putWord32le
  {-# INLINE word32L #-}
  word32B = B.putWord32be
  {-# INLINE word32B #-}
  word64L = B.putWord64le
  {-# INLINE word64L #-}
  word64B = B.putWord64be
  {-# INLINE word64B #-}
  byteString = B.putByteString
  {-# INLINE byteString #-}
#if MIN_VERSION_binary(0,8,1)
  shortByteString = B.putShortByteString
  {-# INLINE shortByteString #-}
#endif
  lazyByteString = B.putLazyByteString
  {-# INLINE lazyByteString #-}
#if MIN_VERSION_binary(0,8,3)
  builder = B.putBuilder
  {-# INLINE builder #-}
#endif

instance Serializer S.Put where
  word8 = S.putWord8
  {-# INLINE word8 #-}
  word16L = S.putWord16le
  {-# INLINE word16L #-}
  word16B = S.putWord16be
  {-# INLINE word16B #-}
  word32L = S.putWord32le
  {-# INLINE word32L #-}
  word32B = S.putWord32be
  {-# INLINE word32B #-}
  word64L = S.putWord64le
  {-# INLINE word64L #-}
  word64B = S.putWord64be
  {-# INLINE word64B #-}
  byteString = S.putByteString
  {-# INLINE byteString #-}
  shortByteString = S.putShortByteString
  {-# INLINE shortByteString #-}
  lazyByteString = S.putLazyByteString
  {-# INLINE lazyByteString #-}
  builder = S.putBuilder
  {-# INLINE builder #-}

-- | Serialize an usigned 16-bit integer in host byte order.
word16H ∷ Serializer s ⇒ Word16 → s
#ifdef WORDS_BIGENDIAN
word16H = word16B
#else
word16H = word16L
#endif
{-# INLINE word16H #-}

-- | Serialize an unsigned 32-bit integer in host byte order.
word32H ∷ Serializer s ⇒ Word32 → s
#ifdef WORDS_BIGENDIAN
word32H = word32B
#else
word32H = word32L
#endif
{-# INLINE word32H #-}

-- | Serialize an unsigned 64-bit integer in host byte order.
word64H ∷ Serializer s ⇒ Word64 → s
#ifdef WORDS_BIGENDIAN
word64H = word64B
#else
word64H = word64L
#endif
{-# INLINE word64H #-}

-- | Serialize an unsigned native-sized integer in serializer default
--   byte order.
word ∷ Serializer s ⇒ Word → s
#ifdef WORDS_BIGENDIAN
word = wordB
#else
word = wordL
#endif
{-# INLINE word #-}

-- | Serialize an unsigned native-sized integer in little endian.
wordL ∷ Serializer s ⇒ Word → s
#if WORD_SIZE_IN_BITS == 32
wordL = word32L . fromIntegral
#else
wordL = word64L . fromIntegral
#endif
{-# INLINE wordL #-}

-- | Serialize an unsigned native-sized integer in big endian.
wordB ∷ Serializer s ⇒ Word → s
#if WORD_SIZE_IN_BITS == 32
wordB = word32B . fromIntegral
#else
wordB = word64B . fromIntegral
#endif
{-# INLINE wordB #-}

-- | Serialize an unsigned native-sized integer in host byte order.
wordH ∷ Serializer s ⇒ Word → s
#if WORD_SIZE_IN_BITS == 32
wordH = word32H . fromIntegral
#else
wordH = word64H . fromIntegral
#endif
{-# INLINE wordH #-}

-- | Serialize a signed 8-bit integer.
int8 ∷ Serializer s ⇒ Int8 → s
int8 = word8 . fromIntegral
{-# INLINE int8 #-}

-- | Serialize a signed 16-bit integer in serializer default byte order.
int16 ∷ Serializer s ⇒ Int16 → s
int16 = word16 . fromIntegral
{-# INLINE int16 #-}

-- | Serialize a signed 16-bit integer in little endian.
int16L ∷ Serializer s ⇒ Int16 → s
int16L = word16L . fromIntegral
{-# INLINE int16L #-}

-- | Serialize a signed 16-bit integer in big endian.
int16B ∷ Serializer s ⇒ Int16 → s
int16B = word16B . fromIntegral
{-# INLINE int16B #-}

-- | Serialize a signed 16-bit integer in host byte order.
int16H ∷ Serializer s ⇒ Int16 → s
#ifdef WORDS_BIGENDIAN
int16H = int16B
#else
int16H = int16L
#endif
{-# INLINE int16H #-}

-- | Serialize a signed 32-bit integer in serializer default byte order.
int32 ∷ Serializer s ⇒ Int32 → s
int32 = word32 . fromIntegral
{-# INLINE int32 #-}

-- | Serialize a signed 32-bit integer in little endian.
int32L ∷ Serializer s ⇒ Int32 → s
int32L = word32L . fromIntegral
{-# INLINE int32L #-}

-- | Serialize a signed 32-bit integer in big endian.
int32B ∷ Serializer s ⇒ Int32 → s
int32B = word32B . fromIntegral
{-# INLINE int32B #-}

-- | Serialize a signed 32-bit integer in host byte order.
int32H ∷ Serializer s ⇒ Int32 → s
#ifdef WORDS_BIGENDIAN
int32H = int32B
#else
int32H = int32L
#endif
{-# INLINE int32H #-}

-- | Serialize a signed 64-bit integer in serializer default byte order.
int64 ∷ Serializer s ⇒ Int64 → s
int64 = word64 . fromIntegral
{-# INLINE int64 #-}

-- | Serialize a signed 64-bit integer in little endian.
int64L ∷ Serializer s ⇒ Int64 → s
int64L = word64L . fromIntegral
{-# INLINE int64L #-}

-- | Serialize a signed 64-bit integer in big endian.
int64B ∷ Serializer s ⇒ Int64 → s
int64B = word64B . fromIntegral
{-# INLINE int64B #-}

-- | Serialize a signed 64-bit integer in host byte order.
int64H ∷ Serializer s ⇒ Int64 → s
#ifdef WORDS_BIGENDIAN
int64H = int64B
#else
int64H = int64L
#endif
{-# INLINE int64H #-}

-- | Serialize a signed native-sized integer in serializer default byte order.
int ∷ Serializer s ⇒ Int → s
#ifdef WORDS_BIGENDIAN
int = intB
#else
int = intL
#endif
{-# INLINE int #-}

-- | Serialize a signed native-sized integer in little endian.
intL ∷ Serializer s ⇒ Int → s
#if WORD_SIZE_IN_BITS == 32
intL = word32L . fromIntegral
#else
intL = word64L . fromIntegral
#endif
{-# INLINE intL #-}

-- | Serialize a signed native-sized integer in big endian.
intB ∷ Serializer s ⇒ Int64 → s
#if WORD_SIZE_IN_BITS == 32
intB = word32B . fromIntegral
#else
intB = word64B . fromIntegral
#endif
{-# INLINE intB #-}

-- | Serialize a signed native-sized integer in host byte order.
intH ∷ Serializer s ⇒ Int → s
#if WORD_SIZE_IN_BITS == 32
intH = word32H . fromIntegral
#else
intH = word64H . fromIntegral
#endif
{-# INLINE intH #-}

-- | Serializer wrapper with little endian default byte order.
newtype LittleEndianSerializer s = LittleEndianSerializer { serializeL ∷ s }
                                   deriving (Typeable, Data, Generic, Monoid)

instance Serializer s ⇒ Serializer (LittleEndianSerializer s) where
  endian _ = LittleEndian
  {-# INLINE endian #-}
  word8 = LittleEndianSerializer . word8
  {-# INLINE word8 #-}
  word16 = LittleEndianSerializer . word16L
  {-# INLINE word16 #-}
  word32 = LittleEndianSerializer . word32L
  {-# INLINE word32 #-}
  word64 = LittleEndianSerializer . word64L
  {-# INLINE word64 #-}
  word16L = LittleEndianSerializer . word16L
  {-# INLINE word16L #-}
  word16B = LittleEndianSerializer . word16B
  {-# INLINE word16B #-}
  word32L = LittleEndianSerializer . word32L
  {-# INLINE word32L #-}
  word32B = LittleEndianSerializer . word32B
  {-# INLINE word32B #-}
  word64L = LittleEndianSerializer . word64L
  {-# INLINE word64L #-}
  word64B = LittleEndianSerializer . word64B
  {-# INLINE word64B #-}
  byteString = LittleEndianSerializer . byteString
  {-# INLINE byteString #-}
  shortByteString = LittleEndianSerializer . shortByteString
  {-# INLINE shortByteString #-}
  lazyByteString = LittleEndianSerializer . lazyByteString
  {-# INLINE lazyByteString #-}
  builder = LittleEndianSerializer . builder
  {-# INLINE builder #-}

-- | Serializer wrapper with big endian default byte order.
newtype BigEndianSerializer s = BigEndianSerializer { serializeB ∷ s }
                                deriving (Typeable, Data, Generic, Monoid)

instance Serializer s ⇒ Serializer (BigEndianSerializer s) where
  endian _ = BigEndian
  {-# INLINE endian #-}
  word8 = BigEndianSerializer . word8
  {-# INLINE word8 #-}
  word16 = BigEndianSerializer . word16B
  {-# INLINE word16 #-}
  word32 = BigEndianSerializer . word32B
  {-# INLINE word32 #-}
  word64 = BigEndianSerializer . word64B
  {-# INLINE word64 #-}
  word16L = BigEndianSerializer . word16L
  {-# INLINE word16L #-}
  word16B = BigEndianSerializer . word16B
  {-# INLINE word16B #-}
  word32L = BigEndianSerializer . word32L
  {-# INLINE word32L #-}
  word32B = BigEndianSerializer . word32B
  {-# INLINE word32B #-}
  word64L = BigEndianSerializer . word64L
  {-# INLINE word64L #-}
  word64B = BigEndianSerializer . word64B
  {-# INLINE word64B #-}
  byteString = BigEndianSerializer . byteString
  {-# INLINE byteString #-}
  shortByteString = BigEndianSerializer . shortByteString
  {-# INLINE shortByteString #-}
  lazyByteString = BigEndianSerializer . lazyByteString
  {-# INLINE lazyByteString #-}
  builder = BigEndianSerializer . builder
  {-# INLINE builder #-}

-- | Force the default byte order.
serializeIn ∷ Serializer s ⇒ Endian → (∀ s' . (Serializer s') ⇒ s') → s
serializeIn LittleEndian = serializeL
serializeIn BigEndian    = serializeB
{-# INLINE serializeIn #-}

-- | Force the default byte order to be the host byte order.
serializeH ∷ Serializer s ⇒ (∀ s' . (Serializer s') ⇒ s') → s
#ifdef WORDS_BIGENDIAN
serializeH = serializeB
#else
serializeH = serializeL
#endif
{-# INLINE serializeH #-}

-- | Serializable type. 'put' must work under assumption that it will be
--   followed by more output.
class Serializable α where
  put ∷ Serializer s ⇒ α → s

instance Serializable Bool where
  put False = word8 0
  put True  = word8 1
  {-# INLINE put #-}

instance Serializable Word8 where
  put = word8
  {-# INLINE put #-}

instance Serializable Word16 where
  put = word16
  {-# INLINE put #-}

instance Serializable Word32 where
  put = word32
  {-# INLINE put #-}

instance Serializable Word64 where
  put = word64
  {-# INLINE put #-}

instance Serializable Word where
  put = word
  {-# INLINE put #-}

instance Serializable Int8 where
  put = int8
  {-# INLINE put #-}

instance Serializable Int16 where
  put = word16 . fromIntegral
  {-# INLINE put #-}

instance Serializable Int32 where
  put = word32 . fromIntegral
  {-# INLINE put #-}

instance Serializable Int64 where
  put = word64 . fromIntegral
  {-# INLINE put #-}

instance Serializable Int where
  put = int
  {-# INLINE put #-}

instance (Serializable α, Serializable β) ⇒ Serializable (α, β) where
  put (a, b) = put a <> put b
  {-# INLINE put #-}

instance Serializable α ⇒ Serializable (Maybe α) where
  put Nothing  = word8 0
  put (Just a) = word8 1 <> put a

instance (Serializable α, Serializable β) ⇒ Serializable (Either α β) where
  put (Left a)  = word8 0 <> put a
  put (Right b) = word8 1 <> put b

instance Serializable BS.ByteString where
  put bs = int (BS.length bs) <> byteString bs

instance Serializable SBS.ShortByteString where
  put bs = int (SBS.length bs) <> shortByteString bs

-- | Serialize a value using the provided default byte order.
putIn ∷ (Serializer s, Serializable α) ⇒ Endian → α → s
putIn e a = serializeIn e (put a)
{-# INLINE putIn #-}

-- | Serialize a value using little endian as the default byte order.
putL ∷ (Serializer s, Serializable α) ⇒ α → s
putL a = serializeL (put a)
{-# INLINE putL #-}

-- | Serialize a value using big endian as the default byte order.
putB ∷ (Serializer s, Serializable α) ⇒ α → s
putB a = serializeB (put a)
{-# INLINE putB #-}

-- | Serialize a value using host byte order as the default byte order.
putH ∷ (Serializer s, Serializable α) ⇒ α → s
putH a = serializeH (put a)
{-# INLINE putH #-}

-- | Types with fixed serialized size.
class Serializable α ⇒ SizedSerializable α where
  -- | Serialized size in bytes.
  size ∷ Proxy α → Int

instance SizedSerializable Bool where
  size _ = 1
  {-# INLINE size #-}

instance SizedSerializable Word8 where
  size _ = 1
  {-# INLINE size #-}

instance SizedSerializable Word16 where
  size _ = 2
  {-# INLINE size #-}

instance SizedSerializable Word32 where
  size _ = 4
  {-# INLINE size #-}

instance SizedSerializable Word64 where
  size _ = 8
  {-# INLINE size #-}

instance SizedSerializable Word where
#if WORD_SIZE_IN_BITS == 32
  size _ = 4
#else
  size _ = 8
#endif
  {-# INLINE size #-}

instance SizedSerializable Int8 where
  size _ = 1
  {-# INLINE size #-}

instance SizedSerializable Int16 where
  size _ = 2
  {-# INLINE size #-}

instance SizedSerializable Int32 where
  size _ = 4
  {-# INLINE size #-}

instance SizedSerializable Int64 where
  size _ = 8
  {-# INLINE size #-}

instance SizedSerializable Int where
#if WORD_SIZE_IN_BITS == 32
  size _ = 4
#else
  size _ = 8
#endif
  {-# INLINE size #-}

instance (SizedSerializable α, SizedSerializable β)
         ⇒ SizedSerializable (α, β) where
  size _ = size (Proxy ∷ Proxy α) + size (Proxy ∷ Proxy β)
  {-# INLINE size #-}

-- | Serializable type. 'putRest' must work under assumption that it will not
--   be followed by any more output.
class RestSerializable α where
  putRest ∷ Serializer s ⇒ α → s

instance RestSerializable BS.ByteString where
  putRest = byteString
  {-# INLINE putRest #-}

instance RestSerializable SBS.ShortByteString where
  putRest = shortByteString
  {-# INLINE putRest #-}

instance RestSerializable LBS.ByteString where
  putRest = lazyByteString
  {-# INLINE putRest #-}

instance RestSerializable BB.Builder where
  putRest = builder
  {-# INLINE putRest #-}

instance Serializable α ⇒ RestSerializable [α] where
  putRest = mconcat . fmap put
  {-# INLINE putRest #-}

instance (Serializable α, RestSerializable β) ⇒ RestSerializable (α, β) where
  putRest (a, b) = put a <> putRest b
  {-# INLINE putRest #-}
