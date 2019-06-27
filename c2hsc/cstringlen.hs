{-# LINE 1 "cstringlen.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module HsFoo where

import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad





data CStringArrayLen = CStringArrayLen { 
    length :: Int
  , cstringArray :: Ptr CString
} deriving Show

instance Storable CStringArrayLen where
    sizeOf    _ = (16)
{-# LINE 22 "cstringlen.hsc" #-}
    alignment _ = 8
{-# LINE 23 "cstringlen.hsc" #-}
    
    poke p cstringArrayLen = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ length cstringArrayLen
{-# LINE 26 "cstringlen.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ cstringArray cstringArrayLen
{-# LINE 27 "cstringlen.hsc" #-}

    peek p = return CStringArrayLen
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 30 "cstringlen.hsc" #-}
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
{-# LINE 31 "cstringlen.hsc" #-}
