module Test.Main where

import Data.IntMap (IntMap)
import Data.IntMap (empty, insert, lookup, delete, keys) as IntMap

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.Array (length, sort, nub) as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheckGen, arbitrary, Result (..))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Data.Argonaut (encodeJson, decodeJson)
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int32BE (..))


main :: Effect Unit
main = do
  log "IntMap"
  log " - insert exists"
  quickCheckGen insertExistsIntMap
  log " - delete doesn't exist"
  quickCheckGen deleteDoesntExistIntMap
  log " - sorted and deduped keys have same length"
  quickCheckGen uniqueKeysIntMap
  log " - json iso"
  quickCheckGen jsonIsoIntMap
  log " - arraybuffer iso"
  quickCheckGen abIsoIntMap


insertExistsIntMap :: Gen Result
insertExistsIntMap = do
  xs <- genIntMap
  k <- arbitrary
  a <- arbitrary
  pure $ case IntMap.lookup k (IntMap.insert k a xs) of
    Nothing -> Failed "Key doesn't exist"
    Just a'
      | a == a' -> Success
      | otherwise -> Failed $ "Values don't match - inserted: " <> show a <> ", found: " <> show a'

deleteDoesntExistIntMap :: Gen Result
deleteDoesntExistIntMap = do
  xs <- genIntMap
  k <- arbitrary
  a <- arbitrary
  pure $ case IntMap.lookup k (IntMap.delete k (IntMap.insert k a xs)) of
    Nothing -> Success
    Just a' -> Failed $ "Found value after deletion - inserted: " <> show a <> ", found: " <> show a'


uniqueKeysIntMap :: Gen Result
uniqueKeysIntMap = do
  xs <- genIntMap
  let ks = IntMap.keys xs
      ks' = Array.nub (Array.sort ks)
  pure $ if Array.length ks' == Array.length ks
          then Success
          else Failed $ "Key lengths differ after sorting and deduping - original: " <> show ks <> ", sorted: " <> show ks'


jsonIsoIntMap :: Gen Result
jsonIsoIntMap = do
  set <- genIntMap
  pure $ case decodeJson (encodeJson set) of
    Left e -> Failed $ "Json decoding failed: " <> e
    Right set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'

abIsoIntMap :: Gen Result
abIsoIntMap = do
  set <- genIntMap'
  let ab = unsafePerformEffect (encodeArrayBuffer set)
      mSet' = unsafePerformEffect (decodeArrayBuffer ab)
  pure $ case mSet' of
    Nothing -> Failed "ArrayBuffer decoding failed"
    Just set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'


genIntMap :: Gen (IntMap Int)
genIntMap = do
  xs <- arrayOf (Tuple <$> arbitrary <*> arbitrary)
  pure (foldr (\(Tuple k x) -> IntMap.insert k x) IntMap.empty xs)

genIntMap' :: Gen (IntMap Int32BE)
genIntMap' = do
  xs <- arrayOf (Tuple <$> arbitrary <*> (Int32BE <$> arbitrary))
  pure (foldr (\(Tuple k x) -> IntMap.insert k x) IntMap.empty xs)
