{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A group of directories to work on
module Multidir.Selection (
  Selection(..),
  applySelection,
  optSelection,
  parseSelection,
  ) where

import Prelude hiding ((&&), (||), not)
import Data.Algebra.Boolean
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import System.FilePath

import Multidir.ReadConfig


-- | A hand-rolled Alternative-like functionality for a parser
orElse :: Monoid e => (a -> Either e b) -> (a -> Either e b) -> (a -> Either e b)
orElse f g x = case f x of
  Right y -> Right y
  Left e1 -> case g x of
    Right y -> Right y
    Left e2 -> Left (e1 <> e2)


newtype Selection = Selection {
  select :: FilePath -> Proj -> Bool
} deriving (Boolean)

(\\) :: Selection -> Selection -> Selection
x \\ y = x && not y

tagSelection :: Text -> Selection
tagSelection t = Selection $ \_ -> elem t . projTags

defaultSelection :: Selection
defaultSelection = tagSelection "default"

isSubdir :: FilePath -> FilePath -> Bool
isSubdir x y = splitDirectories x `isPrefixOf` splitDirectories y

dirSelection :: FilePath -> Selection
dirSelection p = Selection $ \d q ->
  isSubdir (normalise (d </> p)) (projDir q)


-- | Legal characters for tags
isTagChar :: Char -> Bool
isTagChar c = isAlphaNum c || c == '-' || c == '_' || c == '@'


parseSelection :: String -> Either String Selection
parseSelection = let

  initialExpr :: String -> Either String (Selection, String)
  initialExpr = expr `orElse` continue defaultSelection

  expr :: String -> Either String (Selection, String)
  expr a = do
    (x, b) <- atom a
    continue x b

  continue :: Selection -> String -> Either String (Selection, String)
  continue x ('|':a) = do
    (y, b) <- atom a
    continue (x || y) b
  continue x ('&':a) = do
    (y, b) <- atom a
    continue (x && y) b
  continue x ('\\':a) = do
    (y, b) <- atom a
    continue (x \\ y) b
  continue x s = Right (x, s)

  atom :: String -> Either String (Selection, String)
  atom ('!':a) = do
    (x, b) <- atom a
    pure (not x, b)
  atom ('(':a) = do
    (x, b) <- expr a
    case b of
      (')':c) -> Right (x, c)
      u -> Left ("Expected closed bracket but saw: " <> u)
  atom ('{':a) = let
    f ('}':b) = Right ("",b)
    f (c:b) = first (c:) <$> f b
    f "" = Left ("Expected a close brace")
    in first dirSelection <$> f a
  atom a@(u:a')
    | isTagChar u = let
        (us,v) = span isTagChar a'
        in Right (tagSelection (T.pack (u:us)), v)
    | otherwise = Left ("Expected an item, got " <> a)
  atom "" = Left "Expected an item, got nothing"

  go s = case initialExpr s of
    Left e -> Left e
    Right (x, "") -> Right x
    Right (_, r) -> Left ("Unexpected input in selection: " <> r)

  in go


applySelection :: Selection -> FilePath -> Projs -> Projs
applySelection (Selection f) d (Projs t) = Projs (filter (f d) t)


optSelection :: Parser Selection
optSelection = let
  reader = eitherReader parseSelection
  mods =
    long "select" <>
    short 's' <>
    help "a set of projects to operate on" <>
    value defaultSelection <>
    metavar "SELECTION"
  in option reader mods
