{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Android.Log
    ( logV
    , logD
    , logI
    , logW
    , logE
    , logFatal
    ) where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data Android_LogPriority
    = ANDROID_LOG_UNKNOWN
    | ANDROID_LOG_DEFAULT
    | ANDROID_LOG_VERBOSE
    | ANDROID_LOG_DEBUG
    | ANDROID_LOG_INFO
    | ANDROID_LOG_WARN
    | ANDROID_LOG_ERROR
    | ANDROID_LOG_FATAL
    | ANDROID_LOG_SILENT
    deriving (Show, Eq)

instance Enum Android_LogPriority where
    succ ANDROID_LOG_UNKNOWN = ANDROID_LOG_DEFAULT
    succ ANDROID_LOG_DEFAULT = ANDROID_LOG_VERBOSE
    succ ANDROID_LOG_VERBOSE = ANDROID_LOG_DEBUG
    succ ANDROID_LOG_DEBUG = ANDROID_LOG_INFO
    succ ANDROID_LOG_INFO = ANDROID_LOG_WARN
    succ ANDROID_LOG_WARN = ANDROID_LOG_ERROR
    succ ANDROID_LOG_ERROR = ANDROID_LOG_FATAL
    succ ANDROID_LOG_FATAL = ANDROID_LOG_SILENT
    succ ANDROID_LOG_SILENT = error "Android_LogPriority.succ: ANDROID_LOG_SILENT has no successor"
    pred ANDROID_LOG_DEFAULT = ANDROID_LOG_UNKNOWN
    pred ANDROID_LOG_VERBOSE = ANDROID_LOG_DEFAULT
    pred ANDROID_LOG_DEBUG = ANDROID_LOG_VERBOSE
    pred ANDROID_LOG_INFO = ANDROID_LOG_DEBUG
    pred ANDROID_LOG_WARN = ANDROID_LOG_INFO
    pred ANDROID_LOG_ERROR = ANDROID_LOG_WARN
    pred ANDROID_LOG_FATAL = ANDROID_LOG_ERROR
    pred ANDROID_LOG_SILENT = ANDROID_LOG_FATAL
    pred ANDROID_LOG_UNKNOWN = error "Android_LogPriority.pred: ANDROID_LOG_UNKNOWN has no predecessor"
    enumFromTo from to = go from
      where
        end = fromEnum to
        go v =
            case compare (fromEnum v) end of
                LT -> v : go (succ v)
                EQ -> [v]
                GT -> []
    enumFrom from = enumFromTo from ANDROID_LOG_SILENT
    fromEnum ANDROID_LOG_UNKNOWN = 0
    fromEnum ANDROID_LOG_DEFAULT = 1
    fromEnum ANDROID_LOG_VERBOSE = 2
    fromEnum ANDROID_LOG_DEBUG = 3
    fromEnum ANDROID_LOG_INFO = 4
    fromEnum ANDROID_LOG_WARN = 5
    fromEnum ANDROID_LOG_ERROR = 6
    fromEnum ANDROID_LOG_FATAL = 7
    fromEnum ANDROID_LOG_SILENT = 8
    toEnum 0 = ANDROID_LOG_UNKNOWN
    toEnum 1 = ANDROID_LOG_DEFAULT
    toEnum 2 = ANDROID_LOG_VERBOSE
    toEnum 3 = ANDROID_LOG_DEBUG
    toEnum 4 = ANDROID_LOG_INFO
    toEnum 5 = ANDROID_LOG_WARN
    toEnum 6 = ANDROID_LOG_ERROR
    toEnum 7 = ANDROID_LOG_FATAL
    toEnum 8 = ANDROID_LOG_SILENT
    toEnum unmatched = error ("Android_LogPriority.toEnum: Cannot match " ++ show unmatched)

android_log_print :: Android_LogPriority -> String -> String -> IO Int
android_log_print prio tag msg = do
    t <- newCString tag
    f <- newCString "%s"
    m <- newCString msg
    __android_log_print (fromEnum prio) t f m

android_log_assert :: String -> String -> String -> IO ()
android_log_assert cond tag msg = do
    c <- newCAString cond
    t <- newCAString tag
    f <- newCString "%s"
    m <- newCAString msg
    __android_log_assert c t f m

logV :: String -> String -> IO Int
logV = android_log_print ANDROID_LOG_VERBOSE

logD :: String -> String -> IO Int
logD = android_log_print ANDROID_LOG_DEBUG

logI :: String -> String -> IO Int
logI = android_log_print ANDROID_LOG_INFO

logW :: String -> String -> IO Int
logW = android_log_print ANDROID_LOG_WARN

logE :: String -> String -> IO Int
logE = android_log_print ANDROID_LOG_ERROR

logFatal :: String -> String -> IO Int
logFatal = android_log_print ANDROID_LOG_FATAL

logAssert :: Bool -> String -> String -> String -> IO ()
logAssert True cond tag ms = android_log_assert cond tag ms
logAssert False _ _ _ = return ()

{-
    int __android_log_print(int prio, const char *tag,  const char *fmt, ...)
-}
foreign import capi "android/log.h __android_log_print" __android_log_print
    :: Int -> CString -> CString -> CString -> IO Int

{-
    void __android_log_assert(const char *cond, const char *tag, const char *fmt, ...)
-}
foreign import capi "android/log.h __android_log_assert" __android_log_assert
    :: CString -> CString -> CString -> CString -> IO ()