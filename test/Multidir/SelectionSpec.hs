{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Multidir.SelectionSpec (spec) where

import qualified Data.Map as M
import Multidir.ReadConfig
import Multidir.Selection
import Test.Hspec


{-
shouldBeLeft :: Show b => (a -> IO ()) -> Either a b -> IO ()
shouldBeLeft f = \case
  Left x -> f x
  Right y -> expectationFailure ("was meant to be Left: " <> show y)
-}

shouldBeRight :: Show a => (b -> IO ()) -> Either a b -> IO ()
shouldBeRight f = \case
  Left x -> expectationFailure ("was meant to be Right: " <> show x)
  Right y -> f y

shouldMatch :: String -> FilePath -> Proj -> IO ()
shouldMatch s workingDir proj = let
  t (Selection f) = (workingDir, proj) `shouldSatisfy` uncurry f
  in shouldBeRight t $ parseSelection s

shouldNotMatch :: String -> FilePath -> Proj -> IO ()
shouldNotMatch s workingDir proj = let
  t (Selection f) = (workingDir, proj) `shouldNotSatisfy` uncurry f
  in shouldBeRight t $ parseSelection s

shouldNotParse :: String -> IO ()
shouldNotParse s = case parseSelection s of
  Left _ -> pure ()
  Right _ -> expectationFailure ("Should not parse: " <> s)

spec :: Spec
spec = do

  describe "single tag" $ do
    it "should find a tag (1)" $
      shouldMatch "tagA" "/dir1" (Proj "/dir2" ["tagA", "tagB"] M.empty)
    it "should find a tag (2)" $
      shouldMatch "tagB" "/dir1" (Proj "/dir2" ["tagA", "tagB"] M.empty)
    it "should not find a tag" $
      shouldNotMatch "tagC" "/dir1" (Proj "/dir2" ["tagA", "tagB"] M.empty)

  describe "single dir" $ do
    it "should match directory (subdir)" $
      shouldMatch "{dir11}" "/dir1" (Proj "/dir1/dir11" ["tagA", "tagB"] M.empty)
    it "should match directory (current dir)" $
      shouldMatch "{./}" "/dir1" (Proj "/dir1/dir11" ["tagA", "tagB"] M.empty)
    it "should match directory (absolute dir)" $
      shouldMatch "{/dir1}" "/dir2" (Proj "/dir1/dir11" ["tagA", "tagB"] M.empty)
    it "should not match directory (subdir)" $
      shouldNotMatch "{dir11}" "/dir1" (Proj "/dir1/dir12" ["tagA", "tagB"] M.empty)
    it "should not match directory (absolute dir)" $
      shouldNotMatch "{/dir2}" "/dir2" (Proj "/dir1/dir12" ["tagA", "tagB"] M.empty)

  describe "combinators" $ do
    it "or (1)" $
      shouldMatch "tagA|tagB" "/dir1" (Proj "/dir2" ["tagA", "tagC"] M.empty)
    it "or (2)" $
      shouldMatch "tagB|tagC" "/dir1" (Proj "/dir2" ["tagA", "tagC"] M.empty)
    it "or (3)" $
      shouldNotMatch "tagA|tagB" "/dir1" (Proj "/dir2" ["tagC", "tagD"] M.empty)
    it "and (1)" $
      shouldMatch "tagA&tagB" "/dir1" (Proj "/dir2" ["tagA", "tagB"] M.empty)
    it "and (2)" $
      shouldNotMatch "tagA&tagC" "/dir1" (Proj "/dir2" ["tagA", "tagB"] M.empty)
    it "and (3)" $
      shouldNotMatch "tagA&tagC" "/dir1" (Proj "/dir2" ["tagB", "tagC"] M.empty)
    it "not (1)" $
      shouldMatch "!tagA" "/dir1" (Proj "/dir2" ["tagB"] M.empty)
    it "not (1)" $
      shouldNotMatch "!tagA" "/dir1" (Proj "/dir2" ["tagA"] M.empty)

  describe "miscellaneous parsing failures" $ do
    it "double operators" $ do
      shouldNotParse "&&"
      shouldNotParse "x&&"
      shouldNotParse "&&y"
      shouldNotParse "x&&y"
