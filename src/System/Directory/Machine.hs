{-# LANGUAGE Rank2Types #-}
module System.Directory.Machine where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.Machine
import System.Directory
import System.FilePath ((</>))
import System.IO.Machine

import System.Directory.Machine.Internal

directoryWalk :: ProcessT IO FilePath FilePath
directoryWalk = MachineT . return $ Await (\root -> f [root] []) Refl stopped where
  f []    []      = directoryWalk
  f xs    (y:ys)  = MachineT . return $ Yield y $ f xs ys
  f (x:xs) []      = MachineT $ do
    ys <- getDirectoryContents x
    let contents = (x </>) <$> (filter isDirectoryContentsValid $ ys)
    dirs <- filterM doesDirectoryExist contents
    runMachineT $ f (dirs ++ xs) contents

directoryContents :: ProcessT IO FilePath [FilePath]
directoryContents = auto (filter isDirectoryContentsValid) <~ autoM getDirectoryContents

directories :: ProcessT IO FilePath FilePath
directories = filteredIO doesDirectoryExist

files :: ProcessT IO FilePath FilePath
files = filteredIO doesFileExist

