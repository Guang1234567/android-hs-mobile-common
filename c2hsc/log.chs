{-# LANGUAGE ForeignFunctionInterface #-}

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "log.h"

{#enum android_LogPriority {upcaseFirstLetter} deriving (Show, Eq) #}
