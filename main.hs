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
import qualified Data.MultiSet       as MS
import           Data.Ord
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Encoding        as TE
import qualified Data.Tree           as Tree
import           System.Environment
import           System.IO
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B

type AnagramWord = Text
type Dictionary = [AnagramWord]
type LetterSet = MS.MultiSet Char
type AnagramStep = (MS.MultiSet AnagramWord, LetterSet)

anagram = sort "poultry outwits ants"

interestingAnagramWord :: AnagramWord -> Bool
interestingAnagramWord "a" = True
interestingAnagramWord "i" = True
interestingAnagramWord s = T.length s > 1

loadDictionary :: FilePath -> IO Dictionary
loadDictionary file =
  (sortBy (flip (comparing T.length)) .
   filter interestingAnagramWord .
   T.lines) <$>
  TIO.readFile file

toLetterSet :: AnagramWord -> LetterSet
toLetterSet = MS.fromList . T.unpack . T.toLower

extractAnagramWord :: LetterSet -> AnagramWord -> Maybe LetterSet
extractAnagramWord letterSet word =
  if MS.isSubsetOf wordSet letterSet
     then Just (MS.difference letterSet wordSet)
     else Nothing
  where wordSet = toLetterSet word

unfoldAnagram :: Dictionary -> AnagramStep -> (AnagramStep, [AnagramStep])
unfoldAnagram dictionary anagram@(wordsSoFar,letterSetSoFar) =
  (anagram,anagrams')
  where anagrams' =
          mapMaybe anagramStep dictionary
        anagramStep newAnagramWord =
          case extractAnagramWord letterSetSoFar newAnagramWord of
            Nothing -> Nothing
            Just newLetterSet ->
              Just (MS.insert newAnagramWord wordsSoFar,newLetterSet)

anagrams :: Dictionary -> AnagramWord -> [[AnagramWord]]
anagrams dictionary word =
  map (MS.toList . fst) $
  filter (MS.null . snd) $
  Tree.flatten $
  Tree.unfoldTree (unfoldAnagram dictionary)
                  (MS.empty,toLetterSet word)


findMatches ws phrase = anagrams ws (T.pack $ filter (not . isSpace) phrase)

printMatch phrase =
  case md5hash `elem` hashes of
    True -> putStrLn $ "Match Found" ++ md5hash ++ " - " ++ show phrase
    False -> putStrLn $ md5hash ++ " - " ++ show phrase
  where
    md5hash = (show . md5 . B.pack) phrase
    hashes = ["e4820b45d2277f3844eac66c903e84be"
              , "23170acc097c24edb98fc5488ab033fe"
              , "665e5bcb0c20062fe8abaaf4628bb154"]



main :: IO ()
main =
  do [phrase] <- getArgs
     hSetBuffering stdout NoBuffering
     ws <- loadDictionary "wordlist"
     mapM_ (printMatch . T.unpack . T.unwords) $
       findMatches ws phrase
