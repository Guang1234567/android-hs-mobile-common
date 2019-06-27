{-# LANGUAGE ForeignFunctionInterface #-}

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "cstringlen.h"

data CStringArrayLen
{#pointer *cstringArrayLen as CStringArrayLenPtr -> CStringArrayLen #}
