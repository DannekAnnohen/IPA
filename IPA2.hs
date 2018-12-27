{-
International Phonetic Alphabet
Robert Max Polter
09 Dec 2018 - work in progress

Issues:
- Tone has heuristic implementation
IPA (Extra High Tone) = ThisFile (Falling Tone)
IPA (Extra Low Tone) = ThisFIle (Rising Tone)

- the matching of IPA symbols and specs is not robustly implemented - the lists rely heavily on their ordering
-> Nothing as a value for lookup xInventory should be dead (can be circumvented?)

To Do:
1. look into Voigtländer paper on bidirectionalization for spellout function 
2. find better solution for xSym and xSpec Lists; make expandable w/o disturbing matching - safe as tuples & xSym? matching should be robust!!! +++ downgrading mSpec & maybe also c/vSpec?
3. non-heuristic tone implementation
4. deal with non-superscript diacritics/symbols
5. specify show for data IPA
-. non-pulmonic consonants
-. more suprasegmentals
-. affricates
-. "other symbols"

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
- improved on type system

Changes 26.12.2018
- parser now merges sounds with diacritics
- solved typing dilemma
- added exceptions to parser


-}
module IPA2

  (
-- accessing functions for consonants and vowels
  manner, place, voicing,
  openness, frontness, roundedness,

-- parsers
  parseIPA, parse1,
  )

where

-- I use this package to implement IPA symbol-meaning mapping dictionaries. A qualified import means that whenever I call a function from this package I have to write it like so : Map.function
-- strict implementation is used because boths lists and value-datatypes are relatively small
import qualified Data.Map.Strict as Map
import IPA_Data2

place :: IPA -> Place
place (IPA (C place _ _) _) = place

manner :: IPA -> Manner
manner (IPA (C _ manner _) _) = manner

voicing :: IPA -> Voicing
voicing (IPA (C _ _ voicing) _) = voicing

openness :: IPA -> Openness
openness (IPA (V open _ _) _) = open

frontness :: IPA -> Frontness
frontness (IPA (V _ front _) _) = front

roundedness :: IPA -> Roundedness
roundedness (IPA (V _ _ rounded) _) = rounded

typing :: IPA -> Typing
typing (IPA (C _ _ _) _) = Consonant
typing (IPA (V _ _ _) _) = Vowel
typing (IPAb _) = Boundary
typing (IPAm _) = Diacritic


-- here is a linearly operating converter that converts IPA-Strings into a list of consonants, vowels and white spaces, it was precisely as annoying to code as it looks

parseIPA :: [Char] -> [IPA]
parseIPA str = spellback str [] [] []

mergeIPA :: [Sound] -> [Modif] -> IPA
mergeIPA (x:xs) dia = IPA x dia
mergeIPA [] dia | dia == [] = Unreadable
                | otherwise = error "Modifier attached to nothing!"


spellback :: [Char] -> [Sound] -> [Modif] -> [IPA] -> [IPA]
spellback [] zw dia out | zw /= [] = out ++ [mergeIPA zw dia]
                        | otherwise = out
spellback (x:xs) zw dia out | x `elem` iSym = case Map.lookup x iInventory of
  Just temp -> if zw == [] then spellback xs [temp] dia out 
    else spellback xs [temp] [] (out ++ [mergeIPA zw dia])

  Nothing   -> if zw == [] then spellback xs [UnreadableSound] dia out 
    else spellback xs [UnreadableSound] [] (out ++ [mergeIPA zw dia])

                            | x `elem` mSym = if zw /= [] then case Map.lookup x mInventory of
  Just temp -> spellback xs zw (dia ++ [temp]) out 
  Nothing   -> spellback xs zw (dia ++ [UnreadableModif]) out
    else error "Modifier attached to nothing!"

                            | x `elem` bSym = case Map.lookup x bInventory of
  Just temp -> spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [temp])
  Nothing   -> spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [Unreadable])

                            | otherwise = spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [Unreadable])


parse1 :: Char -> IPA
parse1 x        | x `elem` iSym = case Map.lookup x iInventory of
      Just temp -> IPA temp []
      Nothing   -> Unreadable

                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> IPAm temp
      Nothing   -> Unreadable

                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp
      Nothing   -> Unreadable

                | otherwise = Unreadable

-- spellout :: [IPA] -> [Char]



-- this is just for playing around, parses a String into a vertical list of IPA descriptions, doesn't yet support diacritics, suprasegmentals or tone because I was tired





{-

spellback (x:xs) zw dia out | x `elem` iSym && zw == [] = case Map.lookup x iInventory of
                  Just temp -> spellback xs [temp] dia out
                  Nothing   -> spellback xs [UnreadableSound] dia out

                            | x `elem` iSym && zw /= [] = case Map.lookup x iInventory of
                  Just temp -> spellback xs [temp] dia (out ++ [mergeIPA zw dia])
                  Nothing   -> spellback xs [UnreadableSound] dia (out ++ [mergeIPA zw dia])

                            | x `elem` mSym && zw /= [] = case Map.lookup x mInventory of
                  Just temp -> spellback xs zw (dia ++ [temp]) out
                  Nothing   -> spellback xs zw (dia ++ [UnreadableModif]) out

                            | x `elem` mSym && zw == [] = error "no"

                            | x `elem` bSym = case Map.lookup x bInventory of
                  Just temp -> spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [temp])
                  Nothing   -> spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [Unreadable])

                            | otherwise = spellback xs [] [] (out ++ [mergeIPA zw dia] ++ [Unreadable])

-- old code, possibly obsolete typings


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

-}

