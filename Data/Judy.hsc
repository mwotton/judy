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
-- population (number of elements). Note that as Judy is allocated on
-- C language side, GHC's profiling system won't report memory use by
-- Judy arrays.
--
-- For further references to the implementation, see:
--
-- * <http://en.wikipedia.org/wiki/Judy_array>
--
-- Building a simple word-index table. About 4x faster than using an 'IntMap'
--
-- >
-- > import Control.Monad
-- > import qualified Data.Judy as J
-- >
-- > main = do
-- >    j <- J.new :: IO (J.JudyL Int)
-- >    forM_ [1..10000000] $ \n -> J.insert n (fromIntegral n :: Int) j
-- >    v <- J.lookup 100 j
-- >    print v
-- >
--
-- Running this:
--
-- > $ ghc -O2 --make A.hs
-- > [1 of 1] Compiling Main             ( A.hs, A.o )
-- > Linking A ...
--
-- > $ time ./A
-- > Just 100
-- > ./A  1.95s user 0.08s system 99% cpu 2.028 total
--
--    /By default this library is threadsafe/.
--    
--    /Multiple Haskell threads may operate on the arrays simultaneously. You can compile without locks if you know you're running in a single threaded fashion with: cabal install -funsafe/
--
module Data.Judy (

    -- * Basic types
    JudyL, Key

    -- * Operations
    , Data.Judy.new
    , Data.Judy.null
    , Data.Judy.size
    , Data.Judy.insert
    , Data.Judy.lookup
    , Data.Judy.member
    , Data.Judy.delete

    -- adjust
    -- update

--    memoryUsed

    -- * Judy-storable types
    , JE(..)

  ) where

#if !defined(UNSAFE)
import Control.Concurrent
#endif
import Control.Applicative ((<$>))

import Foreign hiding (new)
import Foreign.C.Types
import Foreign.ForeignPtr

import GHC.Ptr
import GHC.Base
import GHC.Prim
import GHC.Word

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

-- | Allocate a new empty JudyL array. A finalizer is associated with
-- the JudyL array, that will free it automatically once the last
-- reference has been dropped. Note that if you store pointers in the
-- Judy array we have no way of deallocating those -- you'll need to track
-- those yourself (e.g. via StableName or ForeignPtr).
--
-- The Haskell GC will track references to the foreign resource, but the 
-- foreign resource won't exert any heap pressure on the GC, meaning
-- that finalizers will be run much later than you expect. An explicit
-- 'performGC' can help with this.
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

-- TODO: fuse construction with uvectors.

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
                        v_word <- peek v_ptr
                        return . Just =<< fromWord v_word
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

-- | /O(1), null. Is the map empty? 
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

-- TODO: fromList
-- TODO: toList
-- TODO: update

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

------------------------------------------------------------------------
--
-- - Could be any Haskell value thanks to StablePtr
-- - ST-based interface
-- - Freeze/Pure interface.
--
-- TODO: make it thread safe.
--
-- TODO: hash interface based on the document (cache hash, C function).
-- IntMap interface.
-- Split out basic interface.
--
-- Binary instance?
--
-- Fast bytestrings.
-- Performance benchmarks.
-- Type families to pick different underlying representations.
--
