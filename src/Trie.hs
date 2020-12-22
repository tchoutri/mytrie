{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Trie where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T

data Trie = Bottom | Internal (HashMap Char Trie)
  deriving (Eq, Show)

empty :: Trie
empty = Internal HM.empty

member :: Text -> Trie -> Bool
member text = go (T.head text) (T.tail text)
  where
    go :: Char -> Text -> Trie -> Bool
    go x xs (Internal  hm) | T.null xs =
      case HM.lookup x hm of
        Nothing -> False
        Just _  -> True
    go x xs (Internal  hm) = 
      let newHead = T.head xs
          newTail = T.tail xs
       in Just True == (go newHead newTail <$> HM.lookup x hm)
    go _ _ Bottom = False

insert :: Text -> Trie -> Trie
insert text = go (T.head text) (T.tail text) 
  where
    go :: Char -> Text -> Trie -> Trie
    go x xs (Internal hm) =
      if T.null xs
      then Internal $ HM.insert x Bottom hm
      else
        let newHead = T.head xs
            newTail = T.tail xs
         in Internal $ HM.alter (Just . go newHead newTail . fromMaybe empty) x hm
    go x xs Bottom = go x xs empty

trie1 :: Trie
trie1 = 
        insert "ace"
      . insert "act"
      . insert "bad"
      . insert "bake"
      . insert "bat"
      . insert "batter"
      . insert "cab"
      . insert "cat"
      . insert "catnap"
      . insert "catnip"
      $ empty
