{-# LANGUAGE MagicHash                #-}

#include "MachDeps.h"

-- | Very fast, mutable associative data types based on Judy arrays.
--
-- A good imperative, mutable replacement for IntMap.
--
-- Judy arrays are both speed- and memory-efficient, with no tuning or
-- configuration required, across a wide range of index set types (sequential,
-- periodic, clustered, random). Judy\'s speed and memory usage are typically
-- better than other data storage models such as skiplists, linked lists,
-- binary, ternary, b-trees, or even hashing, and improves with very large
-- data sets.
--
-- The memory used by a Judy array is nearly proportional to the
-- population (number of elements).
--
-- For further references to the implementation, see:
--
-- * <http://en.wikipedia.org/wiki/Judy_array>
--
-- /Examples/:
--
-- Generate 1 million random integers. Report the largest one we see.
--
-- > import System.Random.Mersenne
-- > import qualified Data.Judy as J
-- > import Control.Monad
-- >
-- > main = do
-- >    g  <- getStdGen
-- >    rs <- randoms g
-- >    j  <- J.new :: IO (J.JudyL Int)
-- >    forM_ (take 1000000 rs) $ \n ->
-- >        J.insert n 1 j
-- >    v  <- J.findMax j
-- >    case v of
-- >         Nothing    -> print "Done."
-- >         Just (k,_) -> print k
--
-- Compile it:
--
-- > $ ghc -O2 --make Test.hs
--
-- Running it:
--
-- > $ time ./Test
-- > 18446712059962695226
-- > ./Test  0.65s user 0.03s system 99% cpu 0.680 total
--
-- /Notes/:
--
-- * /By default this library is threadsafe/.
--
-- * /Multiple Haskell threads may operate on the arrays simultaneously. You can compile without locks if you know you're running in a single threaded fashion with: cabal install -funsafe/
--
-- Sun Sep 27 17:12:24 PDT 2009: The library has only lightly been tested.
--
module Data.Judy (

    -- * Basic types
    Data.Judy.Internal.JudyL
    , Data.Judy.Internal.Key
    , Data.Judy.JudyImmutable(..)

    -- * Construction
    , Data.Judy.new

    -- * Queries
    , Data.Judy.null
    , Data.Judy.size
    , Data.Judy.member
    , Data.Judy.lookup

    -- * Insertion and removal
    , Data.Judy.insert
    , Data.Judy.insertWith
    , Data.Judy.delete
    , Data.Judy.adjust

    -- * Min/Max
    , Data.Judy.findMin
    , Data.Judy.findMax

    -- * Conversion
    , Data.Judy.keys
    , Data.Judy.elems
    , Data.Judy.toList
    -- , Data.Judy.memoryUsed

    -- * Freezing
    , Data.Judy.freeze
    , Data.Judy.unsafeFreeze

    -- * Judy-storable types
    , Data.Judy.JE (toWord, fromWord)
  ) where

#if !defined(UNSAFE)
import Control.Concurrent
#endif
import Control.Exception (evaluate)
import System.IO.Unsafe()

import Foreign hiding (new)

import GHC.Ptr
import GHC.Base
#if MIN_VERSION_base(4,10,0)
import GHC.Float
#endif
import GHC.Prim()
import Data.Char(chr)
import Data.Judy.Internal

--
-- For instances
--
import qualified Data.ByteString as S

------------------------------------------------------------------------

newtype JudyImmutable a = JudyImmutable (JudyL a)

-- | Allocate a new empty JudyL array.
--
-- A finalizer is associated with the JudyL array, that will cause the
-- garbage collector to free it automatically once the last reference
-- has been dropped on the Haskell side.
--
-- /Note: The Haskell GC will track references to the foreign resource, but the foreign resource won't exert any heap pressure on the GC, meaning that finalizers will be run much later than you expect. An explicit 'performGC' can help with this./
--
-- /Note: that if you store pointers in the Judy array we have no way of deallocating those -- you'll need to track those yourself (e.g. via StableName or ForeignPtr)/
--
new :: JE a => IO (JudyL a)
new = do
    -- we allocate the structure on the Haskell heap (just a pointer)
    fp <- mallocForeignPtrBytes (sizeOf (undefined :: Ptr Word))

    -- note that the Haskell GC doesn't really know costly the arrays are.
    addForeignPtrFinalizer c_judyl_free_ptr fp
    withForeignPtr fp $ \p -> poke p (castPtr nullPtr)

#if defined(UNSAFE)
    return $! JudyL fp
#else
    -- and make it threadsafe.
    mv <- newMVar fp
    return $! JudyL mv
#endif

------------------------------------------------------------------------

-- | Insert a key and value pair into the JudyL array.
-- Any existing key will be overwritten.
--
insert :: JE a => Key -> a -> JudyL a -> IO ()
insert k v m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        v_ptr <- c_judy_lins p (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else poke v_ptr =<< toWord v
{-# INLINE insert #-}

-- | Insert with a function, combining new value and old value.
--
-- * If the key does not exist in the map, the value will be inserted.
-- * If the key does exist, the combining function will be applied: f new old
insertWith :: JE a => (a -> a -> a) -> Key -> a -> JudyL a -> IO ()
insertWith f k v m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q     <- peek p -- get the actual judy array
        v_ptr1 <- c_judy_lget q (fromIntegral k) nullError
        if v_ptr1 == judyErrorPtr then memoryError
            else if v_ptr1 == nullPtr
                    then do
                        v_ptr2 <- c_judy_lins p (fromIntegral k) nullError
                        if v_ptr2 == judyErrorPtr then memoryError
                          else poke v_ptr2 =<< toWord v
                    else do
                        old_v <- fromWord =<< peek v_ptr1
                        new_v <- toWord (f v old_v)
                        poke v_ptr1 new_v
{-# INLINE insertWith #-}

------------------------------------------------------------------------

-- | Lookup a value associated with a key in the JudyL array. Return
-- Nothing if no value is found.
lookup :: JE a => Key -> JudyL a -> IO (Maybe a)
lookup k m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q     <- peek p -- get the actual judy array
        v_ptr <- c_judy_lget q (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else if v_ptr == nullPtr
                    then return Nothing
                    else do
                        v <- fromWord =<< peek v_ptr
                        return . Just $! v
{-# INLINE lookup #-}

-- | Is the key a member of the map?
member :: Key -> JudyL a -> IO Bool
member k m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q     <- peek p -- get the actual judy array
        v_ptr <- c_judy_lget q (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else return $! v_ptr /= nullPtr
{-# INLINE member #-}

-- | Update a value at a specific key with the result of the provided
-- function. When the key is not a member of the map, no change is made.
adjust :: JE a => (a -> a) -> Key -> JudyL a -> IO ()
adjust f k m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q     <- peek p -- get the actual judy array
        v_ptr <- c_judy_lget q (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else if v_ptr == nullPtr
                    then return ()
                    else do
                        old_v <- fromWord =<< peek v_ptr
                        new_v <- toWord (f old_v)
                        poke v_ptr new_v
{-# INLINE adjust #-}


-- | Delete the Index\/Value pair from the JudyL array.
--
delete :: Key -> JudyL a -> IO ()
delete k m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        i <- c_judy_ldel p (fromIntegral k) nullError
        if i == judyError then memoryError else return ()
{-# INLINE delete #-}



-- | /O(1)/, null. Is the map empty?
null :: JudyL a -> IO Bool
null m = (== 0) <$> size m
{-# INLINE null #-}

-- | /O(1)/, size. The number of elements in the map.
size :: JudyL a -> IO Int
size m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array
        r <- c_judy_lcount q 0 (-1) nullError
        return $! fromIntegral r
{-# INLINE size #-}

------------------------------------------------------------------------
-- Iteration


-- |
-- > JLF(PValue, PJLArray, Index) // JudyLFirst()
--
-- Search (inclusive) for the first index present that is equal to or greater
-- than the passed Index. (Start with Index = 0 to find the first index in the
-- array.) JLF() is typically used to begin a sorted-order scan of the indexes
-- present in a JudyL array.
--
-- If successful, Index is returned set to the found index, and PValue is
-- returned set to a pointer to Index's Value. If unsuccessful, PValue is returned
-- set to NULL, and Index contains no useful information. PValue must be tested
-- for non-NULL prior to using Index, since a search failure is possible.
--

-- | findMin. Find the minimal key, and its associated value, in the map.
-- Nothing if the map is empty.
--
findMin :: JE a => JudyL a -> IO (Maybe (Key, a))
findMin m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array
        alloca $ \k_ptr -> do
            poke k_ptr 0 -- start at 0
            v_ptr <- c_judy_lfirst q k_ptr nullError
            if v_ptr == nullPtr
                then return Nothing -- empty
                else do
                    v <- fromWord =<< peek v_ptr
                    k <- peek k_ptr
                    return . Just $! (k, v)
{-# INLINE findMin #-}

-- | findMax. Find the maximal key, and its associated value, in the map.
-- Nothing if the map is empty.
--
findMax :: JE a => JudyL a -> IO (Maybe (Key, a))
findMax m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array
        alloca $ \k_ptr -> do
            poke k_ptr (-1) -- start at 0
            v_ptr <- c_judy_llast q k_ptr nullError
            if v_ptr == nullPtr
                then return Nothing -- empty
                else do
                    v <- fromWord =<< peek v_ptr
                    k <- peek k_ptr
                    return . Just $! (k, v)
{-# INLINE findMax #-}

------------------------------------------------------------------------
-- | Makes a copy of a Judy array and packs it into an immutable
--   wrapper.
freeze :: (Show a, JE a) => JudyL a -> IO (JudyImmutable a)
freeze j = do
  newj <- new
  toList' j >>= mapM_ (put newj)
  return $! JudyImmutable newj

  where
    put :: (Show a, JE a) => JudyL a -> (Key,a) -> IO ()
    put arr (k,v) = do
      r <- insert k v arr
      evaluate r
--       print ("inserting", r, k, v)

------------------------------------------------------------------------
-- | Unsafely accesses a judy array. If you never try to update it
--   again, this may be safe, and save some memory. Caveat emptor.
unsafeFreeze :: JE a => JudyL a -> IO (JudyImmutable a)
unsafeFreeze = return . JudyImmutable

------------------------------------------------------------------------
-- | Return all keys of the map in ascending order.
--   It is important that this not be interleaved with updates, so we
--   take a JudyImmutable, which can only be obtained with freeze
--   or unsafeFreeze (if you are sure you know what you are doing).
keys :: JudyImmutable a -> IO [Key]
keys (JudyImmutable m) = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        let go i = (do
                     -- dellocate
                     r <- alloca $ \k_ptr -> do
                         poke k_ptr i
                         v_ptr <- c_judy_lnext q k_ptr nullError
                         if v_ptr == nullPtr
                            then return Nothing
                            else do
                                k <- peek k_ptr
                                return (Just k)

                     case r of
                          Nothing -> return []
                          Just k  -> do xs <- go k
                                        return (k:xs)
                   )


        -- Get the ball rolling with the first valid key
        r <- alloca $ \k_ptr -> do
                poke k_ptr 0
                v_ptr <- c_judy_lfirst q k_ptr nullError
                if v_ptr == nullPtr
                   then return Nothing
                   else do
                        k <- peek k_ptr
                        return $! Just k
        case r of
             Nothing -> return []
             Just k  -> do
                 xs <- go k
                 return (k : xs)

------------------------------------------------------------------------
-- | Return keys and values of the map in ascending order.
toList :: JE a => JudyImmutable a -> IO [(Key,a)]
toList (JudyImmutable m) = toList' m


------------------------------------------------------------------------
-- | Return keys and values of the map in ascending order.
toList' :: JE a => JudyL a -> IO [(Key,a)]
toList' m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        let go i = (do
                     -- dellocate
                     r <- alloca $ \k_ptr -> do
                         poke k_ptr i
                         v_ptr <- c_judy_lnext q k_ptr nullError
                         if v_ptr == nullPtr
                            then return Nothing
                            else do
                                k <- peek k_ptr
                                v <- fromWord =<< peek v_ptr
                                return (Just (k,v))

                     case r of
                          Nothing     -> return []
                          Just (k,v)  -> do xs <- go k
                                            return ((k,v):xs)
                   )


        -- Get the ball rolling with the first valid key
        r <- alloca $ \k_ptr -> do
                poke k_ptr 0
                v_ptr <- c_judy_lfirst q k_ptr nullError
                if v_ptr == nullPtr
                   then return Nothing
                   else do
                        k <- peek k_ptr
                        v <- fromWord =<< peek v_ptr
                        return (Just (k,v))
        case r of
             Nothing -> return []
             Just (k,v)  -> do
                 xs <- go k
                 return ((k,v) : xs)


-- | Return all elems of the map in ascending order.
elems :: JE a => JudyImmutable a -> IO [a]
elems (JudyImmutable m) = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        let go i = (do
                     -- dellocate
                     r <- alloca $ \k_ptr -> do
                         poke k_ptr i
                         v_ptr <- c_judy_lnext q k_ptr nullError
                         if v_ptr == nullPtr
                            then return Nothing
                            else do
                                k <- peek k_ptr
                                v <- fromWord =<< peek v_ptr
                                return (Just (k,v))

                     case r of
                          Nothing     -> return []
                          Just (k,v)  -> do xs <- go k
                                            return (v:xs)
                   )


        -- Get the ball rolling with the first valid key
        r <- alloca $ \k_ptr -> do
                poke k_ptr 0
                v_ptr <- c_judy_lfirst q k_ptr nullError
                if v_ptr == nullPtr
                   then return Nothing
                   else do
                        k <- peek k_ptr
                        v <- fromWord =<< peek v_ptr
                        return (Just (k,v))
        case r of
             Nothing -> return []
             Just (k,v)  -> do
                 xs <- go k
                 return (v : xs)


------------------------------------------------------------------------
-- The JE element class.

--
-- | Class of things that can be stored in the JudyL array.
-- You need to be able to convert the structure to a Word value,
-- or a word-sized pointer.
--
-- /Note: that it is possible to convert any Haskell value into a JE-type, via a StablePtr. This allocates an entry in the runtime's stable pointer table, giving you a pointer that may be passed to C, and that when dereferenced in Haskell will yield the original Haskell value. See the source for an example of this with strict bytestrings./
--
class JE a where
    -- | Convert the Haskell value to a word-sized type that may be stored in a JudyL
    toWord   :: a -> IO Word
    -- | Reconstruct the Haskell value from the word-sized type.
    fromWord :: Word -> IO a

------------------------------------------------------------------------

instance JE () where
    toWord   () = return 0
    fromWord _  = return ()
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Bool where
    toWord     = return . fromIntegral . fromEnum
    fromWord   = return . toEnum . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Ordering where
    toWord     = return . fromIntegral . fromEnum
    fromWord   = return . toEnum . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

------------------------------------------------------------------------

instance JE Word where
    toWord   w = return w
    fromWord w = return w
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Int where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Int8 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Int16 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Int32 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

#if (WORD_SIZE_IN_BITS == 64)
instance JE Int64 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}
#endif

instance JE Word8 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Word16 where
    toWord   w = return (fromIntegral w)
    fromWord w = return (fromIntegral w)
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

instance JE Word32 where
    toWord   = return . fromIntegral
    fromWord = return . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

#if (WORD_SIZE_IN_BITS == 64)
instance JE Word64 where
    toWord   = return . fromIntegral
    fromWord = return . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}
#endif

instance JE Char where
    toWord   = return . fromIntegral . ord
    fromWord = return . chr . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

#if MIN_VERSION_base(4,10,0)

instance JE Float where
    toWord   = toWord . castFloatToWord32
    fromWord = fmap castWord32ToFloat . fromWord
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

#if (WORD_SIZE_IN_BITS == 64)
instance JE Double where
    toWord   = toWord . castDoubleToWord64
    fromWord = fmap castWord64ToDouble . fromWord
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}
#endif

#endif

------------------------------------------------------------------------
-- strict bytestrings may be stored.
--
-- TODO: Quite a bit slower than using an IntMap ( see C.hs , D.hs )
--

instance JE S.ByteString where
    toWord b   = do
        p <- newStablePtr b
        case castStablePtrToPtr p of
             Ptr a## -> return $! W## (int2Word## (addr2Int## a##))

    fromWord w = do
        case fromIntegral w of
             I## i## -> case int2Addr## i## of
                     a## -> deRefStablePtr (castPtrToStablePtr (Ptr a##))
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

