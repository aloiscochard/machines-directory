module System.Directory.Machine.Internal where

isDirectoryContentsValid :: FilePath -> Bool
isDirectoryContentsValid x = (x /= "." && x /= "..")

