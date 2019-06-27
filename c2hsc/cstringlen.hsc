{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module HsFoo where

import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad

#include "cstringlen.h"


data CStringArrayLen = CStringArrayLen { 
    length :: Int
  , cstringArray :: Ptr CString
} deriving Show

instance Storable CStringArrayLen where
    sizeOf    _ = #{size cstringArrayLen}
    alignment _ = #{alignment cstringArrayLen}
    
    poke p cstringArrayLen = do
        #{poke cstringArrayLen, length} p $ length cstringArrayLen
        #{poke cstringArrayLen, cstringArray} p $ cstringArray cstringArrayLen

    peek p = return CStringArrayLen
              `ap` (#{peek cstringArrayLen, length} p)
              `ap` (#{peek cstringArrayLen, cstringArray} p)
