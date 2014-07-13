module System.Directory.Machine.Internal where

import Data.Machine

-- TODO Contribute to machines?
joined :: Process [a] a
joined = f [] where
  f []      = MachineT . return $ Await (\xs -> f xs) Refl stopped
  f (x:xs)  = MachineT . return $ Yield x $ f xs

isDirectoryContentsValid :: FilePath -> Bool
isDirectoryContentsValid x = (x /= "." && x /= "..")

