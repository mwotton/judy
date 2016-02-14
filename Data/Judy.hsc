{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE MagicHash                #-}

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
    JudyL, Key

    -- * Construction
    , Data.Judy.new

    -- * Queries
    , Data.Judy.null
    , Data.Judy.size
    , Data.Judy.member
    , Data.Judy.lookup

    -- * Insertion and removal
    , Data.Judy.insert
--    , Data.Judy.insertWith
    , Data.Judy.delete
    , Data.Judy.adjust

    -- * Min/Max
    , Data.Judy.findMin
    , Data.Judy.findMax

    -- * Conversion
    , Data.Judy.keys
    , Data.Judy.elems
    , Data.Judy.toList
-- memoryUsed

    -- * Judy-storable types
    , JE(..)

  ) where

#if !defined(UNSAFE)
import Control.Concurrent
#endif
import Control.Applicative ((<$>))

import System.IO.Unsafe

import Foreign hiding (new)
import Foreign.C.Types
import Foreign.ForeignPtr

import GHC.Ptr
import GHC.Base
import GHC.Prim
import GHC.Word
import Data.Char(chr)
--
-- For instances
--
import qualified Data.ByteString as S

------------------------------------------------------------------------

#include <Judy.h>

#include <haskell-judy.h>

------------------------------------------------------------------------
-- Type mappings

-- | The type of keys in the JudyL arrays. A word-sized type (64 or 32 bits)
type Key  = Word

-- type Key1 = Word
-- type Key2 = Word
-- type JNth = Word

-- Implementation notes:
--
-- One of the difficulties in using the JudyL function calls lies in
-- determining whether to pass a pointer or the address of a pointer. Since the
-- functions that modify the JudyL array must also modify the pointer to the
-- JudyL array, you must pass the address of the pointer rather than the
-- pointer itself. This often leads to hard-to-debug programmatic errors.
-- In practice, the macros allow the compiler to catch programming
-- errors when pointers instead of addresses of pointers are passed.
--
-- The JudyL function calls have an additional parameter beyond those
-- specified in the macro calls. This parameter is either a pointer to an
-- error structure, or NULL (in which case the detailed error information
-- is not returned).
--
-- JudyL functions: Index is a Word_t and Value is a Word_t. This makes
-- JudyL a pure word-to-word\/pointer mapper. JudySL and JudyHL are
-- based on this property of JudyL.
--

------------------------------------------------------------------------
-- JudyL Arrays

-- | A JudyL array is a mutable, finite map from Word to Word values.
-- It is threadsafe by default.
--
-- A value is addressed by a key. The array may be sparse, and the key may
-- be any word-sized value. There are no duplicate keys.
--
-- Values may be any instance of the JE class.
--
newtype JudyL a =
            JudyL { unJudyL ::
#if !defined(UNSAFE)
                        MVar
#endif
                         (ForeignPtr JudyL_) }

type JudyL_ = Ptr JudyLArray

data JudyLArray

instance Show (JudyL a) where show _ = "<Judy a>"

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

-- | Given a pointer to a JudyL array quickly.
--
-- > Word_t   JudyLFreeArray( PPvoid_t PPJLArray, PJError_t PJError);
--
-- >    JudyLFreeArray(&PJLArray, &JError)
--
-- which is
--
-- >                  #define JLFA(Rc_word, PJLArray) \
-- >                     Rc_word = JudyLFreeArray(&PJLArray, PJE0)
--
foreign import ccall "&hs_judyl_free"
    c_judyl_free_ptr :: FunPtr (Ptr JudyL_ -> IO ())


-- | JudyLMemUsed
--
-- > JudyLMemUsed(PJLArray)
--
-- > #define JLMU(Rc_word, PJLArray) \
-- >   Rc_word = JudyLMemUsed(PJLArray)
-- foreign import ccall "JudyLMemUsed"
--     c_judyl_mem_used :: Ptr JudyL_ -> IO Word

-- | Return the number of bytes of memory used by the JudyL array.
-- memoryUsed :: JudyL -> IO Word
-- memoryUsed j = withForeignPtr (unJudyL j) c_judyl_mem_used

------------------------------------------------------------------------

-- |
-- JLI(PValue, PJLArray, Index) // JudyLIns()
--
-- JudyLIns : insert an index into a 'JudyL' array, returning a pointer
-- to the value to store in the association table (which may be a
-- pointer)
--
-- > JudyLIns(&PJLArray, Index, &JError)
--
-- used as:
--
-- > #define JLI(PValue, PJLArray, Index)  \
-- >   PValue = JudyLIns(&PJLArray, Index, PJE0)
--
-- Insert an Index and Value into the JudyL array PJLArray. If the Index is
-- successfully inserted, the Value is initialized to 0. If the Index was
-- already present, the Value is not modified.
--
foreign import ccall unsafe "JudyLIns"
    c_judy_lins :: Ptr JudyL_ -> Key -> JError -> IO (Ptr Word)

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

{-
-- | Insert with a function, combining new value and old value.
--
-- * If the key does not exist in the map, the value will be inserted.
-- * If the key does exist, the combining function will be applied: f new old
--
insertWith :: JE a => (a -> a -> a) -> Key -> a -> JudyL a -> IO ()
insertWith f k v m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        v_ptr <- c_judy_lins p (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
                --- WRONG!
            else if v_ptr == nullPtr
                    -- not in the map
                    then poke v_ptr =<< toWord v
                    else do
                        old_v <- fromWord =<< peek v_ptr
                        new_v <- toWord (f v old_v)
                        poke v_ptr new_v
{-# INLINE insertWith #-}
-}

------------------------------------------------------------------------

-- |
-- JudyLGet: read a value from a JudyL array
--
-- > JudyLGet(PJLArray, Index, &JError)
--
-- used as:
--
-- > #define JLG(PValue, PJLArray, Index)  \
-- >   PValue = JudyLGet(PJLArray, Index, PJE0)
--
-- Get the pointer PValue associated with Index in the PJLArray Judy array.
-- Return PValue pointing to Value. Return PValue set to NULL if the Index was
-- not present. Return PValue set to PJERR if a malloc() fail occured.
--
foreign import ccall unsafe "JudyLGet"
    c_judy_lget :: JudyL_ -> Key -> JError -> IO (Ptr Word)

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


-- > JudyLDel(&PJLArray, Index, &JError)
--
-- as:
--
-- > #define JLD(Rc_int, PJLArray, Index)  \
-- >    Rc_int = JudyLDel(&PJLArray, Index, PJE0)
--
-- Return Rc_int set to 1 if successful. Return Rc_int set to 0 if Index was
-- not present. Return Rc_int set to JERR if a malloc() fail occured.
--
foreign import ccall unsafe "JudyLDel"
    c_judy_ldel :: Ptr JudyL_ -> Key -> JError -> IO CInt

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


-- |
-- Count the number of indexes present in the JudyL array
--
-- > JLC(Rc_word, PJLArray, Index1, Index2) // JudyLCount()
--
-- Return Rc_word set to the count. A return value of 0 can be valid as a
-- count. To count all indexes present in a JudyL array, use:
--
-- > JLC(Rc_word, PJLArray, 0, -1);
--
-- The type is:
--
-- > JudyLCount(PJLArray, Index1, Index2, &JError)
--
foreign import ccall unsafe "JudyLCount"
    c_judy_lcount :: JudyL_ -> Key -> Key -> JError -> IO CInt

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
foreign import ccall unsafe "JudyLFirst"
    c_judy_lfirst :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

foreign import ccall unsafe "JudyLNext"
    c_judy_lnext :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

foreign import ccall unsafe "JudyLPrev"
    c_judy_lprev :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

foreign import ccall unsafe "JudyLLast"
    c_judy_llast :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)


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

-- | Return all keys of the map, /lazily/, in ascending order.
keys :: JudyL a -> IO [Key]
keys m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        -- Lazily loop through the keys
        let go i = unsafeInterleaveIO (do
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

-- | Return all keys of the map, /lazily/, in ascending order.
toList :: JE a => JudyL a -> IO [(Key,a)]
toList m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        -- Lazily loop through the keys
        let go i = unsafeInterleaveIO (do
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


-- | Return all elems of the map, /lazily/, in ascending order.
elems :: JE a => JudyL a -> IO [a]
elems m = do
#if !defined(UNSAFE)
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
#else
      withForeignPtr (unJudyL m)  $ \p -> do
#endif
        q <- peek p -- get the actual judy array

        -- Lazily loop through the keys
        let go i = unsafeInterleaveIO (do
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
-- Judy errors

{-
-- | Judy error responses.
type JudyErrorCode = CInt

 #{enum JudyErrorCode,
    , judy_error_none           = JU_ERRNO_NONE
    , judy_error_full           = JU_ERRNO_FULL
    , judy_error_nfmax          = JU_ERRNO_NFMAX
    , judy_error_nomem          = JU_ERRNO_NOMEM
    , judy_error_nullparray     = JU_ERRNO_NULLPPARRAY
    , judy_error_nonnullparray  = JU_ERRNO_NONNULLPARRAY
    , judy_error_nullpindex     = JU_ERRNO_NULLPINDEX
    , judy_error_nullpvalue     = JU_ERRNO_NULLPVALUE
    , judy_error_notjudy1       = JU_ERRNO_NOTJUDY1
    , judy_error_notjudyl       = JU_ERRNO_NOTJUDYL
    , judy_error_notjudysl      = JU_ERRNO_NOTJUDYSL
    , judy_error_unsorted       = JU_ERRNO_UNSORTED
    , judy_error_overrun        = JU_ERRNO_OVERRUN
    , judy_error_corrupt        = JU_ERRNO_CORRUPT
    }
-}

newtype JError = JError (Ptr JError_)

data JError_

-- | The null error value.
nullError :: JError
nullError = JError nullPtr

-- | For checking return values from various Judy functions
-- A scalar error.
judyError :: CInt
judyError = (#const JERR)

{-
-- | For checking return values from various Judy functions
-- Pointer to a JError
foreign import ccall unsafe "haskell-judy.h hs_judy_pointer_error"
     c_judy_error_ptr :: Ptr Word

judyErrorPtr :: Ptr Word
judyErrorPtr = c_judy_error_ptr
{-# INLINE judyErrorPtr #-}
-}

-- judyErrorPtr :: Ptr Word
-- judyErrorPtr = Ptr (int2Addr## (word2Int## (not## (int2Word## 0##))))
                           -- wordPtrToPtr
-- {-# INLINE judyErrorPtr #-}

-- | The error pointer. maxBound :: Word. We try hard to get this to inline.
-- Empirically determined to yield the fastest code.
judyErrorPtr :: Ptr Word
judyErrorPtr = Ptr (case (#const PJERR) of I## i## -> int2Addr## i##)
{-# INLINE judyErrorPtr #-}


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

instance JE Char where
    toWord   = return . fromIntegral . ord
    fromWord = return . chr . fromIntegral
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

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

------------------------------------------------------------------------

memoryError :: a
memoryError = error "Data.Judy: memory error with JudyL"
{-# NOINLINE memoryError #-}
