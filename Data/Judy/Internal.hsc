{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Judy.Internal (
    JudyL_, JudyL(..), Key, JError(..), nullError, judyError, judyErrorPtr, memoryError,
    c_judyl_free_ptr, c_judy_lget, c_judy_lins, c_judy_ldel,
    c_judy_lcount, c_judy_lfirst, c_judy_lnext, c_judy_llast
  ) where

#if !defined(UNSAFE)
import Control.Concurrent
#endif
import System.IO.Unsafe()

import Foreign hiding (new)
import Foreign.C.Types
import Foreign.ForeignPtr()

import GHC.Ptr
import GHC.Base
import GHC.Prim()

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


-- |
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


foreign import ccall unsafe "JudyLFirst"
    c_judy_lfirst :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

foreign import ccall unsafe "JudyLNext"
    c_judy_lnext :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

-- foreign import ccall unsafe "JudyLPrev"
--    c_judy_lprev :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

foreign import ccall unsafe "JudyLLast"
    c_judy_llast :: JudyL_ -> Ptr Key -> JError -> IO (Ptr Word)

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

-- {-# INLINE judyErrorPtr #-}

-- | The error pointer. maxBound :: Word. We try hard to get this to inline.
-- Empirically determined to yield the fastest code.
judyErrorPtr :: Ptr Word
judyErrorPtr = Ptr (case (#const PJERR) of (I## i##) -> int2Addr## i##)
{-# INLINE judyErrorPtr #-}


memoryError :: a
memoryError = error "Data.Judy: memory error with JudyL"
{-# NOINLINE memoryError #-}

