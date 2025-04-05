{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Extra functionality for decoding TOML files
module TOML.Decode.Extra (
  decodeTableWith,
  decodeTableWithM,
  getTableOf,
  removeField,
  removeFieldOpt,
  removeFieldWith,
  removeFieldOptWith,
  processTableValuesWith,
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import TOML.Decode
import TOML.Error
import TOML.Value


pop :: Ord k => k -> Map k v -> (Maybe v, Map k v)
pop = M.alterF (,Nothing)

-- | We only want to process a table
decodeTableWithM :: (Table -> DecodeM a) -> Value -> DecodeM a
decodeTableWithM f = \case
  Table t -> f t
  v       -> typeMismatch v

-- | We only want to process a table
decodeTableWith :: (Table -> DecodeM a) -> Decoder a
decodeTableWith = Decoder . decodeTableWithM

-- Now moved upstream
getTableOf :: Decoder a -> Decoder (Map Text a)
getTableOf decoder =
  makeDecoder $ \case
    Table t -> M.traverseWithKey (\k -> addContextItem (Key k) . runDecoder decoder) t
    v -> typeMismatch v

-- | Remove a field, raising error if absent; return rest of table
removeFieldWith :: Decoder a -> Text -> Table -> DecodeM (Table, a)
removeFieldWith d k m = case pop k m of
  (Just v, m') -> (m',) <$> addContextItem (Key k) (unDecoder d v)
  (Nothing, _) -> DecodeM (Left . (, MissingField) . (<> [Key k]))

-- | Remove a field, signalling presence in Maybe; return rest of table
removeFieldOptWith :: Decoder a -> Text -> Table -> DecodeM (Table, Maybe a)
removeFieldOptWith d k m = case pop k m of
  (Just v,  m') -> (m',) . Just <$> addContextItem (Key k) (unDecoder d v)
  (Nothing, m') -> pure (m', Nothing)

-- | Remove a field, raising error if absent; return rest of table
removeField :: DecodeTOML a => Text -> Table -> DecodeM (Table, a)
removeField k m = case pop k m of
  (Just v, m') -> (m',) <$> addContextItem (Key k) (unDecoder tomlDecoder v)
  (Nothing, _) -> DecodeM (Left . (, MissingField) . (<> [Key k]))

-- | Remove a field, signalling presence in Maybe; return rest of table
removeFieldOpt :: DecodeTOML a => Text -> Table -> DecodeM (Table, Maybe a)
removeFieldOpt k m = case pop k m of
  (Just v,  m') -> (m',) . Just <$> addContextItem (Key k) (unDecoder tomlDecoder v)
  (Nothing, m') -> pure (m', Nothing)

-- | Run a decoder on the values of a table
processTableValuesWith :: Decoder a -> Table -> DecodeM (Map Text a)
processTableValuesWith d = M.traverseWithKey (\k -> addContextItem (Key k) . unDecoder d)
