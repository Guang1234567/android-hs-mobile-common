module Android.FileSystem
    ( matchesGlob
    , namesMatching
    ) where

import Android.ForeignUtils
import Android.Log
import Android.Regex.Glob (globToRegex, isPattern)

import Control.Exception (SomeException, handle)
import Control.Monad (forM)

import Foreign
import Foreign.C

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, splitFileName)

import Text.Regex.Posix ((=~))

matchesGlob :: FilePath -> String -> Bool
matchesGlob name pat = name =~ globToRegex pat

_matchesGlobC name glob = do
    name <- peekCString name
    glob <- peekCString glob
    return $ matchesGlob name glob

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
        else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <-
        if null dirName
            then getCurrentDirectory
            else return dirName
    handle (const (return []) :: (SomeException -> IO [String])) $ do
        names <- getDirectoryContents dirName'
        let names' =
                if isHidden pat
                    then filter isHidden names
                    else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <-
        if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
    return
        (if exists
             then [baseName]
             else [])

namesMatching :: FilePath -> IO [FilePath]
namesMatching pat
    | not $ isPattern pat = do
        exists <- doesNameExist pat
        return
            (if exists
                 then [pat]
                 else [])
    | otherwise = do
        case splitFileName pat
            -- 在只有文件名的情况下, 只在当前目录查找.
              of
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            -- 在包含目录的情况下
            (dirName, baseName)
                -- 由于目录本身可能也是一个符合 glob 模式的字符床, 如(/foo*bar/far?oo/abc.txt)
             -> do
                dirs <-
                    if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return [dirName]
                -- 经过上面操作, 拿到所有符合规则的目录
                let listDir =
                        if isPattern baseName
                            then listMatches
                            else listPlain
                pathNames <-
                    forM dirs $ \dir -> do
                        baseNames <- listDir dir baseName
                        return (map (dir </>) baseNames)
                return (concat pathNames)

_namesMatchingC :: CString -> IO (Ptr CStringArrayLen)
_namesMatchingC filePath = do
    filePath' <- peekCString filePath
    pathNames <- namesMatching filePath'
    pathNames' <- forM pathNames newCString :: IO [CString]
    newCStringArrayLen pathNames'

_freeNamesMatching :: Ptr CStringArrayLen -> IO ()
_freeNamesMatching ptr = do
    cstrArrLen <- peekCStringArrayLen ptr
    let cstrArrPtr = getCStringArray cstrArrLen
    freeCStringArray cstrArrPtr
    free ptr
    return ()

foreign export ccall "matchesGlob" _matchesGlobC :: CString -> CString -> IO Bool

foreign export ccall "namesMatching" _namesMatchingC :: CString -> IO (Ptr CStringArrayLen)

foreign export ccall "freeNamesMatching" _freeNamesMatching :: Ptr CStringArrayLen -> IO ()