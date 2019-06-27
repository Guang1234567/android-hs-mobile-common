module Lib
    ( isElfFile
    ) where

import Android.Log
import qualified Data.ByteString.Lazy as BL
import Foreign.C

_hasElfMagic :: BL.ByteString -> Bool
_hasElfMagic content = BL.take 4 content == elfMagic
  where
    elfMagic = BL.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- BL.readFile path
    return $ _hasElfMagic content

_isElfFileC path = do
    path <- peekCString path
    isElfFile path

foreign export ccall "isElfFile" _isElfFileC :: CString -> IO Bool