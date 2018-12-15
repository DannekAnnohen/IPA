{-
International Phonetic Alphabet
Robert Max Polter
09 Dec 2018 - work in progress

Issues:
- Tone has heuristic implementation
IPA (Extra High Tone) = ThisFile (Falling Tone)
IPA (Extra Low Tone) = ThisFIle (Rising Tone)
- diacritics, suprasegmentals and tone symbols are parsed as stand-alone symbols of type Modif, delta-functions in DFAs need to check for potential diacritics at every step of the way or leave them out entirely ):
- the matching of IPA symbols and specs is not robustly implemented - the lists rely heavily on their ordering

To Do:
1. edit parseIPA to merge vowels and consonants with their respective diacritics, change data declaration of IPA TPPP to sth like IPA TPPP [Modif] - or maybe only possible for IPA Cons & IPA Vowel? unlikely here, possible in IPA.hs . implementation via DFA? implementation via second function(merger) on outputlist?
2. find better solution for xSym and xSpec Lists; make expandable w/o disturbing matching - safe as tuples & xSym? matching should be robust!!! +++ downgrading mSpec & maybe also c/vSpec?
3. non-heuristic tone implementation
4. deal with non-superscript diacritics/symbols
5. add function toUnicode
-. non-pulmonic consonants
-. more suprasegmentals
-. affricates
-. "other symbols"
-. specify show for data IPA
-. parseIPA2: name change & optimization

Changes 10.12.2018
- changed names: Open -> Openness, Front -> Frontness, Rounded -> Roundedness (& according functions)
- added export list in module header
- changed name: Symbol -> Boundary, converted to xSym, xSpec, xInventory pattern, adjusted parseIPA & IPA
- exported construction functions to IPA_Construct and raw data & data declarations to IPA_Data

Changes 11.12.2018
- completely changed data-type system
- defined parseIPA for non-IPA Chars and made a non-list version parse1
- made parseIPA more elegant
- improved/ unified feat function
- wrote type checking function

Changes 14.12.2018


THIS MODULE:
- works with an entirely different typing system than the original module
- typing system is significantly more elgant and slimmer
- parser and feat are way more readable
- unfortunately allows mix up of features - a plosive could be rounded ):
- a [Modif] can only be appended indiscriminately to the IPA data declaration of vowels and consonants, there is also no differentiation between diacritics for vowels or consonants
6 data, 6 type <-> original one uses 15 data, 0 type

-}
module IPA2

  (
-- accessing functions for consonants and vowels
  manner, place, voicing,
  openness, frontness, roundedness,
  )

where

-- I use this package to implement IPA symbol-meaning mapping dictionaries. A qualified import means that whenever I call a function from this package I have to write it like so : Map.function
-- strict implementation is used because boths lists and value-datatypes are relatively small
import qualified Data.Map.Strict as Map
import IPA_Data2

feat :: Category -> IPA -> Property
feat x (IPA C p m v) = case x of
  Place        ->  p
  Manner       ->  m
  Voicing      ->  v
feat x (IPA V o f r) = case x of
  Openness     ->  o
  Frontness    ->  f
  Roundedness  ->  r

-- lets you read the roundedness of a consonant, returns voicing - ugly!
feat2 :: Category -> IPA -> Property
feat2 x (IPA _ a b c) = case x of
  Place        ->  a
  Manner       ->  b
  Voicing      ->  c
  Openness     ->  a
  Frontness    ->  b
  Roundedness  ->  c

place :: IPA -> Property
place (IPA C place _ _) = place

manner :: IPA -> Property
manner (IPA C _ manner _) = manner

voicing :: IPA -> Property
voicing (IPA C _ _ voicing) = voicing

openness :: IPA -> Property
openness (IPA V open _ _) = open

frontness :: IPA -> Property
frontness (IPA V _ front _) = front

roundedness :: IPA -> Property
roundedness (IPA V _ _ rounded) = rounded

typing :: IPA -> T
typing (IPA t _ _ _) = t

-- here is a linearly operating converter that converts IPA-Strings into a list of consonants, vowels and white spaces, it was precisely as annoying to code as it looks

parseIPA :: [Char] -> [IPA]
parseIPA [] = []
parseIPA (x:xs) | x `elem` iSym = case Map.lookup x iInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | otherwise = Unreadable : parseIPA xs

parse1 :: Char -> IPA
parse1 x      | x `elem` iSym = case Map.lookup x iInventory of
      Just temp -> temp
      Nothing   -> Unreadable
                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> temp
      Nothing   -> Unreadable
                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp
      Nothing   -> Unreadable
                | otherwise = Unreadable


-- this is just for playing around, parses a String into a vertical list of IPA descriptions, doesn't yet support diacritics, suprasegmentals or tone because I was tired
parseIPA :: [Char] -> [IPA]
parseIPA [] = []
parseIPA (x:xs) | x `elem` iSym = case Map.lookup x iInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp : parseIPA xs
      Nothing   -> Unreadable : parseIPA xs
                | otherwise = Unreadable : parseIPA xs

parseIPA2 :: [Char] -> IO ()
parseIPA2 [] = putStr ""
parseIPA2 (x:xs) | x `elem` iSym = case Map.lookup x iInventory of
      Just temp -> do
        putStrLn $ show temp
        parseIPA2 xs
      Nothing   -> do
        putStrLn $ show Unreadable
        parseIPA2 xs
                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> do 
        putStrLn $ show temp
        parseIPA2 xs
      Nothing   -> do
        putStrLn $ show Unreadable
        parseIPA2 xs
                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> do
        putStrLn $ show temp
        parseIPA2 xs
      Nothing -> do
        putStrLn $ show Unreadable
        parseIPA2 xs
                | otherwise = putStrLn $ show Unreadable
