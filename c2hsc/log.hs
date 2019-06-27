-- GENERATED by C->Haskell Compiler, version 0.28.6 Switcheroo, 25 November 2017 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "log.chs" #-}
{-# LANGUAGE ForeignFunctionInterface #

-}

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable



data android_LogPriority = ANDROID_LOG_UNKNOWN
                         | ANDROID_LOG_DEFAULT
                         | ANDROID_LOG_VERBOSE
                         | ANDROID_LOG_DEBUG
                         | ANDROID_LOG_INFO
                         | ANDROID_LOG_WARN
                         | ANDROID_LOG_ERROR
                         | ANDROID_LOG_FATAL
                         | ANDROID_LOG_SILENT
  deriving (Show,Eq)
instance Enum android_LogPriority where
  succ ANDROID_LOG_UNKNOWN = ANDROID_LOG_DEFAULT
  succ ANDROID_LOG_DEFAULT = ANDROID_LOG_VERBOSE
  succ ANDROID_LOG_VERBOSE = ANDROID_LOG_DEBUG
  succ ANDROID_LOG_DEBUG = ANDROID_LOG_INFO
  succ ANDROID_LOG_INFO = ANDROID_LOG_WARN
  succ ANDROID_LOG_WARN = ANDROID_LOG_ERROR
  succ ANDROID_LOG_ERROR = ANDROID_LOG_FATAL
  succ ANDROID_LOG_FATAL = ANDROID_LOG_SILENT
  succ ANDROID_LOG_SILENT = error "android_LogPriority.succ: ANDROID_LOG_SILENT has no successor"

  pred ANDROID_LOG_DEFAULT = ANDROID_LOG_UNKNOWN
  pred ANDROID_LOG_VERBOSE = ANDROID_LOG_DEFAULT
  pred ANDROID_LOG_DEBUG = ANDROID_LOG_VERBOSE
  pred ANDROID_LOG_INFO = ANDROID_LOG_DEBUG
  pred ANDROID_LOG_WARN = ANDROID_LOG_INFO
  pred ANDROID_LOG_ERROR = ANDROID_LOG_WARN
  pred ANDROID_LOG_FATAL = ANDROID_LOG_ERROR
  pred ANDROID_LOG_SILENT = ANDROID_LOG_FATAL
  pred ANDROID_LOG_UNKNOWN = error "android_LogPriority.pred: ANDROID_LOG_UNKNOWN has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
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
  toEnum unmatched = error ("android_LogPriority.toEnum: Cannot match " ++ show unmatched)

{-# LINE 10 "log.chs" #-}

