module Android.Regex.Glob
    ( globToRegex
    , isPattern
    ) where

globToRegex :: String -> String
globToRegex cs = '^' : _globToRegex cs ++ "$"

_globToRegex :: String -> String
_globToRegex "" = ""
_globToRegex ('*':cs) = ".*" ++ _globToRegex cs
_globToRegex ('?':cs) = '.' : _globToRegex cs
_globToRegex ('[':'!':c:cs) = "[^" ++ c : _charClass cs
_globToRegex ('[':c:cs) = '[' : c : _charClass cs
_globToRegex ('[':_) = error "unterminated character class"
_globToRegex (c:cs) = _escape c ++ _globToRegex cs

_escape :: Char -> String
_escape c
    | c `elem` regexChars = '\\' : [c]
    | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

_charClass :: String -> String
_charClass (']':cs) = ']' : _globToRegex cs
_charClass (c:cs) = c : _charClass cs
_charClass [] = error "unterminated character class"

-- | is glob expression ？
isPattern :: String -> Bool
isPattern = any (`elem` "[?*")