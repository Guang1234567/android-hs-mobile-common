module Android.ForeignUtils
    ( newCStringArray
    , freeCStringArray
    , CStringArrayLen(..)
    , newCStringArrayLen
    , pokeCStringArrayLen
    , peekCStringArrayLen
    ) where

import Foreign
import Foreign.C

import Control.Applicative
import Control.Monad

data CStringArrayLen =
    CStringArrayLen
        { getSize :: Int
        , getCStringArray :: Ptr CString
        }
    deriving (Show)

instance Storable CStringArrayLen where
    sizeOf _ = (16)
    alignment _ = 8
    poke p cstringArrayLen = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ getSize cstringArrayLen
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ getCStringArray cstringArrayLen
    peek p =
        return CStringArrayLen `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p) `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

createCStringArrayLen :: [CString] -> IO CStringArrayLen
createCStringArrayLen cstrs = do
    cstrsPtr <- newCStringArray cstrs
    return $ CStringArrayLen (length cstrs) cstrsPtr

newCStringArrayLen :: [CString] -> IO (Ptr CStringArrayLen)
newCStringArrayLen cstrs = do
    outPtr <- malloc :: IO (Ptr CStringArrayLen)
    cstrArrLen <- createCStringArrayLen cstrs
    poke outPtr $ cstrArrLen
    return outPtr

pokeCStringArrayLen :: Ptr CStringArrayLen -> [CString] -> IO ()
pokeCStringArrayLen outPtr cstrs = do
    cstrArrLen <- createCStringArrayLen cstrs
    poke outPtr cstrArrLen

peekCStringArrayLen :: Ptr CStringArrayLen -> IO CStringArrayLen
peekCStringArrayLen = peek

newCStringArray :: [CString] -> IO (Ptr CString)
--newCStringArray cstrs = newArray pathNames'  -- crash in the furture, if not end with '\0' !!!
-- N.B. the result will be an array of C type char** with the end marked by NULL.
newCStringArray cstrs = newArray0 nullPtr cstrs

freeCStringArray :: Ptr CString -> IO ()
freeCStringArray ptr = do
    pathNames <- peekArray0 nullPtr ptr :: IO [CString]
    forM pathNames free
    free ptr

foreign export ccall "freeOfHaskell" free :: Ptr a -> IO ()

foreign export ccall "freeCStringArrayOfHaskell" freeCStringArray :: Ptr CString -> IO ()