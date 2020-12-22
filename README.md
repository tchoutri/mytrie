# A naïve Trie implementation in Haskell

Many other Haskell implementations of Tries have the structure of the datatype as such:

```Haskell
data Trie = Bottom | Internal (HashMap Char Trie)
```

In this implementation, the `Bottom` constructor represents the end of a path.
Other languages like Python may represent such a thing by using a special character like '\*' pointing to a null value.

For instance, consider the `member` function:

```haskell
member :: Text -> Trie -> Bool
member text = go (T.head text) (T.tail text) -- (1)
  where
    go :: Char -> Text -> Trie -> Bool
    go x xs (Internal  hm) | T.null xs =     -- (2)
      case HM.lookup x hm of
        Nothing -> False
        Just _  -> True
    go x xs (Internal  hm) =                 -- (3)
      let newHead = T.head xs
          newTail = T.tail xs
       in Just True == (go newHead newTail <$> HM.lookup x hm)
       --    ^____ (3c)     ^____ (3b)            ^___ (3a)
    go _ _ Bottom = False                    -- (4)
```

In (1), we delegate the recursion to a helper function called `go`, by explicitly
passing the head and tail of the text.  
(2) shows the case where the tail of the text (`xs`) is empty. This is the moment where
we operate on the last element of the string of text, `x`. The final lookup is
performed to see if the last letter of the string is here, and we return a Boolean
accordingly.

Finally, (3) shows the recursive case.  
In (3a), we perform a lookup in the hashmap to determine if the character `x` we
are currently checking is present. If it is here, it will return a `Just subTrie`
that the `go newHead newTail` partially applied function will use as its last
argument, and recurse. Otherwise, `Nothing` is returned.

The expression in parenthesis that contains (3b) and (3a) has type
`Maybe Bool`. If it returns `Nothing`, it means that a lookup in the body of the 
expression (3a) yielded no value. If it returned `Just False`, it means that we have
arrived at the end of the text we are searching and its last letter is not present
at the current node. If we get `Just True`, it means that the word is entirely
contained whithin the Trie.

And thus, (3c) is the final comparison between what we expect to be a happy result,
`Just True`, and the result of said comparison is the Boolean that we get at the end.

Finally, (4) indicates the moment where we stumble on a too-short word path.  
The word we are looking for is not fully inserted in the Trie.
