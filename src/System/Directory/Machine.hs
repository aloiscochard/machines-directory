{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
module System.Directory.Machine where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM)
import Data.Machine
import System.Directory
import System.FilePath ((</>))
import System.IO.Machine

import System.Directory.Machine.Internal

-- | Recursively (breadth-first) walk thru the directory structure.
--
-- >>> runT (files <~ directoryWalk <~ source ["."])
-- ["./.gitignore",...
directoryWalk :: ProcessT IO FilePath FilePath
directoryWalk = directoryWalk' (const True)

-- | A variant of 'directoryWalk' with a predicate whether to descend
-- into particular directory.
--
-- @
-- directoryWalk' (not . isSuffixOf ".git")
-- @
--
-- @since 0.2.1.0
directoryWalk' :: (FilePath -> Bool) -> ProcessT IO FilePath FilePath
directoryWalk' p = MachineT . return $ Await (\root -> f [root] []) Refl stopped where
  f []     []      = directoryWalk
  f xs     (y:ys)  = MachineT . return $ Yield y $ f xs ys
  f (x:xs) []      = MachineT $ do
    ys <- getDirectoryContents x
    let contents = (x </>) <$> (filter isDirectoryContentsValid $ ys)
    dirs <- filterM doesDirectoryExist (filter p contents)
    runMachineT $ f (dirs ++ xs) contents

directoryContents :: ProcessT IO FilePath [FilePath]
directoryContents = auto (filter isDirectoryContentsValid) <~ autoM getDirectoryContents

directories :: ProcessT IO FilePath FilePath
directories = filteredIO doesDirectoryExist

files :: ProcessT IO FilePath FilePath
files = filteredIO doesFileExist

