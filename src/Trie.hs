{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Trie where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T

data Trie = 
  Trie 
  (HashMap Char Trie)
  deriving (Eq, Show)

empty :: Trie
empty = Trie HM.empty

member :: Text -> Trie -> Bool
member text = go (T.head text) (T.tail text)
  where
    go :: Char -> Text -> Trie -> Bool
    go x xs (Trie  hm) | T.null xs =
      case HM.lookup x hm of
        Nothing -> False
        Just _  -> True
    go x xs (Trie  hm) = 
      let newHead = T.head xs
          newTail = T.tail xs
       in Just True == (go newHead newTail <$> HM.lookup x hm)

insert :: Text -> Trie -> Trie
insert text = go (T.head text) (T.tail text) 
  where
    go :: Char -> Text -> Trie -> Trie
    go x xs (Trie hm) =
      if T.null xs
      then Trie $ HM.insert x (Trie HM.empty) hm
      else
        let newHead = T.head xs
            newTail = T.tail xs
         in Trie $ HM.alter (Just . go newHead newTail . fromMaybe empty) x hm

trie1 :: Trie
trie1 = insert "ace"
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
