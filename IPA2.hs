{-
International Phonetic Alphabet
Robert Max Polter
09 Dec 2018 - work in progress

Naming Decisions:
- no audible release is named Unreleased
- a lateral X is a specific manner of articulation LateralX
- Clicks and implosives are a manner of articulation
- tap/flap is written as tap
- ɗ dental/alveolar implosive written as alveolar
- diacritics for voiced and voiceless are called Envoiced and Devoiced, Dental -> Dentalized (to avoid multiple declarations)

Defaults:
- only symbols of the standard IPA chart are explicitly accounted for, other symbol-diacritic combinations are possible but not automatically recognized (creaky-voiced glottal approximant ʔ̞ will be read as <IPA (C Glottal Plosive Voiceless) [Lowered]> or ʔ̰ as <IPA (C Glottal Plosive Voiceless) [CreakyVoiced]>
- consonants are non-syllabic by default and vowels are syllabic by default. I always thought this was okay, but given how much I have learned doing this it now feels bold. I also do not expect consonants to be modified by "non-syllabic" and vowels to be modified by "syllabic"
- tone can only be carried by syllabic consonants (because apparently that's a thing) and vowels (is that okay?)

Comments:
- Dental, Alveolar (lateral) and PalatoAlveolar Clicks are noted with incorrect unicode symbols in standard ipa table, used this page instead:
https://www.phon.ucl.ac.uk/home/wells/ipa-unicode.htm
- preeffects are not possible atm, but may be with separate parser in the future. it is possible to treat pre-articulations as complex sounds (affricates, diphtongs...)

Issues:
pre-effects:
- need exact syllabic parsing or specific symbol to distinguish pre- and post-articulation effects (pre/post-stopped)
- also:
-- voiced consonants preceded by voiceess prenasalization in Taiwanese and other Southern Min dialects: [m̥b n̥d n̥ɺ ŋ̊ɡ]
-- retroflex prestopped? retroflex poststopped? is it a thing?
-- voiceless (bilabial and other) prestops?
-- symbol for high d̪ in d̪l̪ dentalized voiced prestop and in Kuyani 


Missing Symbols:
- simultaneous ʃ and x - ɧ (the swedish sj-sound)
- only level tones for these tone symbols: '˥','˦','˧','˨','˩' (only readable after a vowel, shouldn't be used)


unique preceding diacritics:
- Downstep and Upstep as syllable-preceding diacritics (syllable-parsing needed)
- non-superscript tone symbols?

Dispute:
- LabialVelar and AlveoloPalatal maybe just Velar + labialized/ Alveolar + Palatalized?

from function typings in IPA_Data
-- in this i assume that there are only complex sounds of the same type. 



To Do:
0. Fix issues (above)
1. look into Voigtländer paper on bidirectionalization for spellout function 
3. write an interpreter that assigns canonical names to special sound-diacritic combinations (e.g. creaky-voiced glottal approximant) + see info / write a merge function for output of non-underlying feature functions
5. complete function present
9. spellchecker for ipa-strings
10. separate metric sequencing or strong integration into parser
11. write warnings for parse1, also convert some error messages into non-fatal warnings (monads?)
12. Account for Leipzig Glossing Rules
13. write separate parser to include pre-articulation effects (prestopped etc.), needs special binding symbol for pre- and post-articulation diacritics (but only to be used with pre)
- write function to easily check parseIPA output for Unreadables 
- write function to show context of unreadable symbol (with spellout/ backparser?)
- this one should include full metric specifications

info for interpreter:
- example: the ExtraShort diacritic turns the epiglottal stop into a flap
- Raised/ Lowered change names


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

Changes 3.1.2019
- added diacritics up to velarized/pharyngealized '\820'

Changes 4.1.2019
- fixed tone implementation
- added diacritics, ingressives, boundaries, "other symbols" (see issues)
- added ejectives separately as standalone IPA instance
- changed name of feature functions to uFeat for underlying representation without modifiers
- added functions: duration, tone, syllabic, phonation, pulmonic, modifiers, modifier, feature functions that return the primary feature and a list of modifiers as a tuple
- changes name parse1 :: Char -> IPA to parseSym, added parse1 :: [Char] -> IPA 

Changes 6.1.2019
- changed typing system to account for affricates and unified all airstreams into one constructor
- implemented specifications for airstreams, adjusted parsers
- added functions airstreams, airstream, deleted function pulmonic

Changes 7.1.2019
- adjusted parsers for multi-sound units, added Untertie as Boundary
- added function complexity
- introduced metInventory for metric sequencing, parser treats metric symbols as standalone boundaries
- fixed most issues
- updated export list
- specified instance Show for IPA and Sound

Changes 9.1.
- cleaned up code
- added function tongueroot, rhotacized, readable, unreadableChar
- added special rhotacized vowel symbols

Changes 10.1.
- changed Unreadable (::IPA) to Unreadable Char
- (abandoned strong integration of pre-articulation effects into main parser, will be separate)
- improved show for IPA to include complex items
- created module IPA_Test

Changes 11.1
- added functions boundary, isBoundary, isComplexity
- changed names for data types:
Sound - sounds, isSound
Modifier - modifiers, containsModifier
Complexity - complexity, isComplexity
Typing - typing, isTyping
Airstream - airstreams, areAirstreams
Boundary - boundary, isBoundary
- improved present
- created class ShowIPA and specified instances for Sound, Airstream and IPA
- made function tyM obsolete and deleted it


-}
module IPA2

  (
-- data
  module IPA_Data2,

-- parsers
  parseIPA, parseSym, parse1, 

-- auxiliary functions
  present,
  )

where

-- I use this package to implement IPA symbol-meaning mapping dictionaries. A qualified import means that whenever I call a function from this package I have to write it like so : Map.function
-- strict implementation is used because boths lists and value-datatypes are relatively small
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import IPA_Data2

-- parser for [Char] to [IPA], view output with function present
-- firstlevel check with xSym seems silly, is it really faster?
-- add warnings about which sound wasn't readable
parseIPA :: [Char] -> [IPA]
parseIPA str = spellback str [] [] []

-- version without mergeIPA reversal but in-order parsing into out, also without preSymbols

spellback :: [Char] -> [(Sound,Airstream)] -> [Modif] -> [IPA] -> [IPA]
spellback [] zw dia out = (out ++ mergeIPA zw dia)

spellback c@(x:xs) zw dia out | x `elem` sSym = if checklast Tiebar dia 
  then spellback xs (zw ++ [sTemp]) dia out 
  else spellback xs [sTemp] [] (out ++ mergeIPA zw dia)

                              | x `elem` xSym = if checklast Tiebar dia
  then spellback xs (flip (:) zw $ fst xTemp) (flip (++) dia $ snd xTemp) out
  else spellback xs [fst xTemp] (snd xTemp) (mergeIPA zw dia ++ out)

                              | x `elem` mSym = spellback xs zw (dia ++ [mTemp]) out

                              | x `elem` bSym = spellback xs [] [] (out ++ mergeIPA zw dia ++ [bTemp])

                              | x == ejm = spellback xs (ejectify zw) dia out

                              | otherwise = spellback xs [] [] (out ++ mergeIPA zw dia ++ [Unreadable x])
-- add warning what is unreadable
  where sTemp = fromJust $ Map.lookup x sInventory
        mTemp = fromJust $ Map.lookup x mInventory
        bTemp = fromJust $ Map.lookup x bInventory
        xTemp = fromJust $ Map.lookup x xInventory
        ejectify = (uncurry (++)) . ejective . splitAt (length zw-1) 
        ejective (xs,[(s,_)]) = (xs,[(s,GlottalicEgressive)])
        checklast i l = if (&&) ((&&) (i `elem` l) $ (==) i $ last l) $ zw /= [] then True else False



mergeIPA :: [(Sound, Airstream)] -> [Modif] -> [IPA]
mergeIPA [] dia = map IPAm $ dia
-- add warning "modifier attached to nothing!"
mergeIPA sa dia = flip (:) [] $ IPA $ tuplify sa $ cut dia

cut [] = [[]]
cut x = case elemIndex Tiebar x of
                Just int -> [take int x] ++ cut (drop (int+1) x)
                Nothing  -> [x]


{-

-- attempt at including pre-effects, test7 is ambiguous, also needs work to pass test4 and test6
spellback :: [Char] -> [(Sound,Airstream)] -> [Modif] -> [IPA] -> [IPA]
spellback [] zw dia out = reverse $ mergeIPA zw dia ++ out

spellback (x: xs) zw dia out | x `elem` sSym = case checkties dia of
  2 -> spellback xs (sTemp : zw) dia (mergeIPA zw (drop 2 dia) ++ out)
  1 -> spellback xs (sTemp : zw) dia out
  0 -> spellback xs [sTemp] [] (mergeIPA zw dia ++ out)

                              | x `elem` xSym = case checkties dia of
  2 -> spellback xs (flip (:) zw $ fst xTemp) (flip (++) dia $ snd xTemp) out
  1 -> spellback xs (flip (:) zw $ fst xTemp) (flip (++) dia $ snd xTemp) out
  0 -> spellback xs [fst xTemp] (snd xTemp) (mergeIPA zw dia ++ out)

                              | x `elem` bSym = spellback xs [] [] (bTemp : mergeIPA zw dia ++ out)

                              | x `elem` preSym = if checkheadelement then case checkties dia of
  2 -> spellback (tail xs) zw (preTemp++dia) out
  1 -> spellback (tail xs) zw (preTemp++dia) out
  0 -> spellback (tail xs) [] preTemp (mergeIPA zw dia ++ out) 
                                else error "pre-modifier attached to nothing"

                              | x `elem` mSym = spellback xs zw (mTemp : dia) out

                              | x == ejm = spellback xs (ejectify zw) dia out

                              | otherwise = spellback xs [] [] ((Unreadable x) : mergeIPA zw dia ++ out)
-- add warning what is unreadable
  where sTemp = fromJust $ Map.lookup x sInventory
        mTemp = fromJust $ Map.lookup x mInventory
        bTemp = fromJust $ Map.lookup x bInventory
        xTemp = fromJust $ Map.lookup x xInventory
        preTemp = [fromJust $ Map.lookup x preInventory, ModifTiebar]
        ejectify = (uncurry (++)) . ejective . splitAt (length zw-1) 
        ejective (xs,[(s,_)]) = (xs,[(s,GlottalicEgressive)])
        checkheadelement = if xs == [] then False else if elem (head xs) tieSym then True else False
        checkties l | (&&) (Tiebar `elem` l) $ zw /= [] = if (==) Tiebar $ head l then 1 else 0
                    | ModifTiebar `elem` l = if (==) ModifTiebar $ l!!1 then 2 else 0
                    | otherwise = 0


-- i chose to reverse the sound and modifier lists here instead of recording them in order in the parser because (++)'s speed only depends on the second list and since out gets bigger. doesn't seem to improve the speed tho for some reason, maybe because of the big reversal in the end? doesnt really make sense
mergeIPA :: [(Sound, Airstream)] -> [Modif] -> [IPA]
mergeIPA [] dia = map IPAm $ reverse dia
-- add warning "modifier attached to nothing!"
mergeIPA sa dia = flip (:) [] $ IPA $ tuplify (reverse sa) $ cut $ reverse dia
 

cut [] = [[]]
cut x = case elemIndex Tiebar x of
                Just int -> [take int x] ++ cut (drop (int+1) x)
                Nothing  -> [x]



-- version without mergeIPA reversal but in-order parsing into out, performance unclear

spellback :: [Char] -> [(Sound,Airstream)] -> [Modif] -> [IPA] -> [IPA]
spellback [] zw dia out = (out ++ mergeIPA zw dia)

spellback (x:xs) zw dia out | x `elem` sSym = case checkties dia of
  2 -> spellback xs [sTemp] dia (out ++ mergeIPA zw (drop 2 dia))
  1 -> spellback xs (zw ++ [sTemp]) dia out 
  0 -> spellback xs [sTemp] [] (out ++ mergeIPA zw dia)

                              | x `elem` xSym = case checkties dia of
  2 -> spellback xs [fst xTemp] (flip (++) dia $ snd xTemp) (out ++ mergeIPA zw dia)
  1 -> spellback xs (flip (:) zw $ fst xTemp) (flip (++) dia $ snd xTemp) out
  0 -> spellback xs [fst xTemp] (snd xTemp) (mergeIPA zw dia ++ out)

                              | x `elem` mSym = if not $ (head xs) `elem` tieSym then spellback xs zw (dia ++ [mTemp]) out
                                else error "modifier attached to nothing"

                              | x `elem` preSym = if (head xs) `elem` tieSym then spellback (tail xs) zw (dia ++ preTemp) out
                                else error "pre-modifier attached to nothing"
                              | x `elem` bSym = spellback xs [] [] (out ++ mergeIPA zw dia ++ [bTemp])

                              | x == ejm = spellback xs (ejectify zw) dia out

                              | otherwise = spellback xs [] [] (out ++ mergeIPA zw dia ++ [Unreadable x])
-- add warning what is unreadable
  where sTemp = fromJust $ Map.lookup x sInventory
        mTemp = fromJust $ Map.lookup x mInventory
        bTemp = fromJust $ Map.lookup x bInventory
        xTemp = fromJust $ Map.lookup x xInventory
        preTemp = [fromJust $ Map.lookup x preInventory, ModifTiebar]
        ejectify = (uncurry (++)) . ejective . splitAt (length zw-1) 
        ejective (xs,[(s,_)]) = (xs,[(s,GlottalicEgressive)])
        checkties l | (&&) (Tiebar `elem` l) $ zw /= [] = if (==) Tiebar $ head l then 1 else 0
                    | ModifTiebar `elem` l = if (==) ModifTiebar $ l!!1 then 2 else 0
                    | otherwise = 0


mergeIPA :: [(Sound, Airstream)] -> [Modif] -> [IPA]
mergeIPA [] dia = map IPAm $ dia
-- add warning "modifier attached to nothing!"
mergeIPA sa dia = flip (:) [] $ IPA $ tuplify sa $ cut dia
  where cut [] = [[]]
        cut x = case elemIndex Tiebar x of
                Just int -> [take int x] ++ cut (drop (int+1) x)
                Nothing  -> [x]

-}




-- parses a single sound
parse1 :: [Char] -> IPA
parse1 str = case length temp of
  0 -> error "input does not constitute a sound"
  _ -> head temp
  where temp = parseIPA str


-- returns the specifications for a single symbol
parseSym :: Char -> IPA
parseSym x        | x `elem` sSym = case Map.lookup x sInventory of
      Just temp -> IPAs temp
      Nothing   -> Unreadable x

                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> IPAm temp
      Nothing   -> Unreadable x

                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp
      Nothing   -> Unreadable x

                | x == ejm = IPAm EjectivityMarker
                | otherwise = Unreadable x

-- formatting [IPA]
present [] = putStr ""
present (x:xs) = case typing x of
  Consonant -> simpleout
  Vowel -> simpleout
  None -> simpleout
  Boundary -> case boundary x of
    Whitespace -> do 
      putStrLn $ "\n\n"++(show x)++"\n\n"
      present xs
    Undertie -> simpleout
    MajorGroup -> do 
      putStrLn $ "\n\n"++(show x)++"\n\n"
      present xs
    MinorGroup -> do
      putStrLn $ "\n"++(show x)++"\n"
      present xs
    PrimaryStress -> simpleout
    SecondaryStress -> simpleout
    SyllableBoundary -> simpleout
    GlobalRise -> simpleout
    GlobalFall -> simpleout
  where simpleout = do {putStrLn $ show x ; present xs}

-- spellout :: [IPA] -> [Char]
-- tbd
