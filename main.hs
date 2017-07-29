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
import           Data.Ord
import           Data.Maybe
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Tree           (Tree)
import qualified Data.Tree           as Tr
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           System.Environment
import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16

import Data.Text.ICU.Char
import Data.Text.ICU.Normalize

type AnagramWord = Text
type Dictionary = Set AnagramWord

type Anagram = MultiSet AnagramWord
type AnagramDict = Map Text [Text]
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, [AnagramWord])

anagrams :: AnagramDict -> Text -> [Text]
anagrams dict source =
  concatMap (buildPossibleAnagrams dict) $ catMaybes $ Tr.flatten $ search dict source

buildPossibleAnagrams :: AnagramDict -> Anagram  -> [Text]
buildPossibleAnagrams dict anagram = map T.unwords variations
  where
    items = catMaybes $ map (\x -> Map.lookup x dict) $ MS.toList anagram
    variations = filter matchesHashes $ concatMap permutations (sequence items)

search :: AnagramDict -> Text -> Tree (Maybe Anagram)
search dict source = Tr.unfoldTree expand initialState
  where initialState = (MS.empty, wordLetters source, dictWords)
        dictWords = reverse $  sortBy (comparing T.length) (Map.keys dict)

extractAnagram :: Anagram -> Text
extractAnagram = T.unwords . MS.toList

expand :: SearchState -> (Maybe Anagram, [SearchState])
expand (wordsSoFar, remaining, dict) = (completeAnagram, nextStates)
  where
    completeAnagram = maybeCompleteAnagram wordsSoFar remaining
    possibleAnagramWords = filter (remaining `containsWord'`) dict
    nextStates = fst $ foldl go ([], possibleAnagramWords) $ possibleAnagramWords
    go (states, d) word =
      ((MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d):states,
       delete word d)

containsWord :: Letters -> Text -> Bool
containsWord letters word = wordLetters word' `MS.isSubsetOf` letters
  where
    word' = T.filter isAlpha $ canonicalForm word

containsWord' :: Letters -> Text -> Bool
containsWord' letters word = wordLetters word' `MS.isSubsetOf` letters
  where
    word' = T.filter isAlpha word

maybeCompleteAnagram :: Anagram -> Letters -> Maybe Anagram
maybeCompleteAnagram wordsSoFar remaining
  | length wordsSoFar > 4 = Nothing
  | MS.null remaining = Just wordsSoFar
  | otherwise = Nothing

matchesHashes :: [Text] -> Bool
matchesHashes anagram = md5hash `elem` hashes
    where
      md5hash = (B.unpack . encode . MD5.hash . B.pack . T.unpack . T.unwords) anagram

hashes :: [String]
hashes = ["25c5b9ee90c7e8e74afe8427b2d7f8d2", "e4820b45d2277f3844eac66c903e84be" , "23170acc097c24edb98fc5488ab033fe" , "665e5bcb0c20062fe8abaaf4628bb154"]

wordLetters :: Text -> Letters
wordLetters = MS.fromList . filter isAlpha . T.unpack . T.toLower

readDictFiltered :: FilePath -> Text -> IO Dictionary
readDictFiltered file phrase = (S.filter goodWord . S.fromList . T.lines) <$> TIO.readFile file
  where goodWord "A" = True
        goodWord "I" = True
        goodWord "O" = True
        goodWord w = T.length w > 1 && containsWord (wordLetters phrase) w

buildAnagarmDict :: Dictionary -> Map Text [Text]
buildAnagarmDict dict = foldr insertWord Map.empty $ S.toList dict
  where
    insertWord word map = Map.insertWith (++) (wordKey word) [word] map
    wordKey word = (T.pack . sort . T.unpack . T.filter isAlpha) $ canonicalForm word

printMatch :: String -> IO ()
printMatch phrase =
  if md5hash `elem` hashes
    then putStrLn $ "Match Found " ++ md5hash ++ " - " ++ show phrase
    else return ()
  where
    md5hash = (B.unpack . encode . MD5.hash . B.pack) phrase

canonicalForm :: Text -> Text
canonicalForm s = noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD s

main :: IO ()
main =
  do (dictionary:phrase:_) <- getArgs
     dict <- readDictFiltered dictionary (T.pack phrase)
     let anagramWords = buildAnagarmDict dict
     print $ "Total words: " ++ show (length dict)
     print $ "Total anagram words: " ++ show (length $ Map.keys anagramWords)
     mapM_(print . T.unpack) $
       take 3 $ anagrams anagramWords (T.pack phrase)
