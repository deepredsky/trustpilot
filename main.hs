{-# LANGUAGE OverloadedStrings #-}

{-

You've made an important decision. Now, let's get to the matter.

We have a message for you. But we hid it.
Unless you know the secret phrase, it will remain hidden.

Can you write the algorithm to find it?

Here is a couple of important hints to help you out:
- An anagram of the phrase is: "poultry outwits ants"
- There are three levels of difficulty to try your skills with
- The MD5 hash of the easiest secret phrase is "e4820b45d2277f3844eac66c903e84be"
- The MD5 hash of the more difficult secret phrase is "23170acc097c24edb98fc5488ab033fe"
- The MD5 hash of the hard secret phrase is "665e5bcb0c20062fe8abaaf4628bb154"
Here is a list of english words, it should help you out.

Type the secret phrase here to see if you found the right one
Trustpilot Development Team
We imagine that right now, you're feeling a bit like Alice. Hmm? Tumbling down the rabbit hole?

-}


module Main where

import           Control.Applicative ((<$>))
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Ord
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Encoding        as TE
import           Data.Tree           (Tree)
import qualified Data.Tree           as Tr
import           Data.Set            (Set)
import qualified Data.Set            as S
import           System.Environment
import           System.IO
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B

type AnagramWord = Text
type Dictionary = Set AnagramWord

type Anagram = MultiSet AnagramWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- The anagrams with the fewest words are returned first, which can lead to
-- high memory usage.
anagrams :: Dictionary -> Text -> [Text]
anagrams dict source =
  map extractAnagram $ catMaybes $ Tr.flatten $ search dict source

search :: Dictionary -> Text -> Tree (Maybe Anagram)
search dict source = Tr.unfoldTree expand initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> Text
extractAnagram = T.unwords . MS.toList

expand :: SearchState -> (Maybe Anagram, [SearchState])
expand (wordsSoFar, remaining, dict) = (completeAnagram, nextStates)
  where
    completeAnagram = if MS.null remaining then Just wordsSoFar else Nothing
    possibleAnagramWords = S.filter (remaining `canSpell`) dict
    -- As we generate new branches, we remove words for which we have
    -- already created a branch: this ensures that independent branches
    -- will not generate identical sets of words.
    nextStates = fst $ foldl go ([], possibleAnagramWords) $ S.toList possibleAnagramWords
    go (states, d) word =
      ((MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d):states,
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters

wordLetters :: Text -> Letters
wordLetters = MS.fromList . filter isAlpha . T.unpack . T.toLower

readDict :: IO Dictionary
readDict = (S.filter goodWord . S.fromList . T.lines) <$> TIO.readFile "wordlist"
  where goodWord "A" = True
        goodWord "I" = True
        goodWord "O" = True
        goodWord w = all isAlpha (T.unpack w)

printMatch phrase =
  if md5hash `elem` hashes
    then putStrLn $ "Match Found " ++ md5hash ++ " - " ++ show phrase
    else return ()
  where
    md5hash = (show . md5 . B.pack) phrase
    hashes = ["e4820b45d2277f3844eac66c903e84be"
              , "23170acc097c24edb98fc5488ab033fe"
              , "665e5bcb0c20062fe8abaaf4628bb154"]


main :: IO ()
main =
  do [phrase] <- getArgs
     dict <- readDict
     mapM_ (printMatch . T.unpack) $
       anagrams dict (T.pack phrase)
