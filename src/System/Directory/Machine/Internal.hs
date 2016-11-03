module System.Directory.Machine.Internal where

-- | Predicate to filter out relative paths: @"."@ and @".."@.
isDirectoryContentsValid :: FilePath -> Bool
isDirectoryContentsValid x = (x /= "." && x /= "..")

