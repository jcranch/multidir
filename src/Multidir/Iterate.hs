module Multidir.Iterate (
  onAll,
  ) where

import Data.Monoid (Ap(..))
import Multidir.ReadConfig


data Results = Results {
  attempts :: Int,
  fails :: Int
}

instance Semigroup Results where
  Results a1 f1 <> Results a2 f2 = Results (a1 + a2) (f1 + f2)

instance Monoid Results where
  mempty = Results 0 0

result :: Bool -> Results
result b = Results {
  attempts = 1,
  fails = if b then 0 else 1
}

reportResults :: Results -> IO ()
reportResults r = let
  f 0 = "all successful"
  f 1 = "1 failure"
  f n = show n <> " failures"
  in putStrLn ("finished: " <> show (attempts r) <> " directories, " <> f (fails r))

-- | Announce the repository
onOne :: (Proj -> IO Bool) -> Proj -> IO Results
onOne f p = do
  putStrLn (projDir p)
  result <$> f p

-- | Takes a function which operates on a project, returning
-- success/failure
onAll :: (Proj -> IO Bool) -> Projs -> IO ()
onAll f (Projs l) = do
  r <- getAp $ foldMap (Ap . onOne f) l
  reportResults r
