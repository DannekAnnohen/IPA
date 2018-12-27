{-

idea: 
downgrade xSpec lists from type IPA to their respective types and create xInventory via function

issue: in data IPA double usage of Modif in IPA Sound [Modif] and IPAm Modif

-}
module IPA_Data2
  (
  IPA(..), Sound(..),
  Manner(..), Place(..), Voicing(..), Frontness(..), Openness(..), Roundedness(..),
-- vowels and consonants
  iInventory, iSym, iSpec,

-- whitespaces, boundaries
  Boundary(..), bSym, bSpec, bInventory,

-- metatype modifiers for diacritics, suprasegmentals & tone
  Modif(..),  mInventory, mSym, mSpec,
  Typing(..),
  )

where

import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as MapL

-- a metatype for IPA symbols
data IPA = IPA Sound [Modif] | IPAb Boundary | IPAm Modif | Unreadable
  deriving (Eq, Show, Read)

data Sound = C Place Manner Voicing | V Openness Frontness Roundedness | UnreadableSound
  deriving (Eq, Show, Read)

data Typing = Consonant | Vowel | Diacritic | Boundary
  deriving (Eq, Show, Read)

-- all possible C or V properties
data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Retroflex | Palatal | Velar | Uvular | Pharyngeal | Glottal
  deriving (Eq, Show, Read)

data Manner = Plosive | Nasal | Trill | Tap | Fricative | Lateralfricative | Approximant | Lateralapproximant
  deriving (Eq, Show, Read)

data Voicing = Voiced | Voiceless
  deriving (Eq, Show, Read)

data Openness = Close | CloseCloseMid | CloseMid | Mid | OpenMid | OpenOpenMid | Open
  deriving (Eq, Show, Read, Ord)

data Frontness = Front | FrontCentral | Central | BackCentral | Back
  deriving (Eq, Show, Read, Ord)

data Roundedness = Rounded | Unrounded
  deriving (Eq, Show, Read)


-- diacritics, so far only diacritics for superscript symbols
-- basic suprasegmentals and tone TONE IS IMPLEMENTED OUTSIDE IPA STANDARD! extra high -> falling, extra low -> rising
data Modif = Aspirated | Labialized | Palatalized | Velarized | Pharyngealized | NasalRelease | LateralRelease | Long | HalfLong | FallingTone | HighTone | MidTone | LowTone | RisingTone | UnreadableModif
  deriving (Eq, Show, Read)

-- we need something to deal with whitespaces, morpheme boundaries (and probably worse ): ) I also derived it from Ord because we might care for a ranking of boundaries once I find it in me to abandon the not-too phonological concept of "whitespace" for sth cooler
data Boundary = Whitespace | SyllableBoundary
  deriving (Eq, Show, Read, Ord)

-- here is the source data for all IPA symbols, some functions may call xSym or xSpec, most should work with the much faster xInventory as it utilizes Data.Map
iInventory = Map.fromList $ tuplify iSym iSpec
mInventory = Map.fromList $ tuplify mSym mSpec
bInventory = Map.fromList $ tuplify bSym bSpec

-- non-mandatory inventorys that may be called under special circumstances are lazily computed to optimize fore memory complexity
cInventory = MapL.fromList $ tuplify cSym cSpec
vInventory = MapL.fromList $ tuplify vSym vSpec
diaInventory = MapL.fromList $ tuplify diaSym diaSpec
supInventory = MapL.fromList $ tuplify supSym supSpec
tonInventory = MapL.fromList $ tuplify tonSym tonSpec

-- creates tuples out of lists, not exported
tuplify :: [a] -> [b] -> [(a, b)]
tuplify (x:[]) (y:[]) = [(x,y)]
tuplify (x:xs) (y:ys) = [(x,y)] ++ tuplify xs ys

-- all symbols and matching specifications in the xSym xSpec format
bSym = [' ', '.']
bSpec = [IPAb Whitespace, IPAb SyllableBoundary]

mSym = diaSym++supSym++tonSym
mSpec = diaSpec++supSpec++tonSpec

diaSym = ['ʰ','ʷ','ʲ','ˠ','ˤ','ⁿ','ˡ']
diaSpec = [Aspirated, Labialized, Palatalized, Velarized, Pharyngealized, NasalRelease, LateralRelease]

supSym = ['ː','ˑ']
supSpec = [Long, HalfLong]

tonSym = ['˥','˦','˧','˨','˩']
tonSpec = [FallingTone, HighTone, MidTone, LowTone, RisingTone]

iSym = cSym++vSym
iSpec = cSpec++vSpec

vSym = ['i','y','ɨ','ʉ','ɯ','u','ɪ','ʏ','ʊ','e','ø','ɘ','ɵ','ɤ','o','ə',
  'ɛ','œ','ɜ','ɞ','ʌ','ɔ','æ','ɐ','a','ɶ','ɑ','ɒ']

cSym = ['p','b','m','ʙ','ɸ','β','ɱ','ⱱ','f','v','ʋ','θ','ð','t','d','n','r','ɾ',
  's','z','ɬ','ɮ','ɹ','l','ʃ','ʒ','ʈ','ɖ','ɳ','ɽ','ʂ','ʐ','ɻ','ɭ','c','ɟ','ɲ','ç','ʝ',
  'j','ʎ','k','g','ŋ','x','ɣ','ɰ','ʟ','q','ɢ','ɴ','ʀ','χ','ʁ','ħ','ʕ','ʔ','h','ɦ']

vSpec = [V Close Front Unrounded,
 V Close Front Rounded,
 V Close Central Unrounded,
 V Close Central Rounded,
 V Close Back Unrounded,
 V Close Back Rounded,
 V CloseCloseMid FrontCentral Unrounded,
 V CloseCloseMid FrontCentral Rounded,
 V CloseCloseMid BackCentral Rounded,
 V CloseMid Front Unrounded,
 V CloseMid Front Rounded,
 V CloseMid Central Unrounded,
 V CloseMid Central Rounded,
 V CloseMid Back Unrounded,
 V CloseMid Back Rounded,
 V Mid Central Unrounded,
 V OpenMid Front Unrounded,
 V OpenMid Front Rounded,
 V OpenMid Central Unrounded,
 V OpenMid Central Rounded,
 V OpenMid Back Unrounded,
 V OpenMid Back Rounded,
 V OpenOpenMid Front Unrounded,
 V OpenOpenMid Central Unrounded,
 V Open Front Unrounded,
 V Open Front Rounded,
 V Open Back Unrounded,
 V Open Back Rounded]

cSpec = [C Bilabial Plosive Voiceless,
 C Bilabial Plosive Voiced,
 C Bilabial Nasal Voiced,
 C Bilabial Trill Voiced,
 C Bilabial Fricative Voiceless,
 C Bilabial Fricative Voiced,
 C Labiodental Nasal Voiced,
 C Labiodental Fricative Voiced,
 C Labiodental Fricative Voiceless,
 C Labiodental Tap Voiced,
 C Labiodental Approximant Voiced,
 C Dental Fricative Voiceless,
 C Dental Fricative Voiced,
 C Alveolar Plosive Voiceless,
 C Alveolar Plosive Voiced,
 C Alveolar Nasal Voiced,
 C Alveolar Trill Voiced,
 C Alveolar Tap Voiced,
 C Alveolar Fricative Voiceless,
 C Alveolar Fricative Voiced,
 C Alveolar Lateralfricative Voiceless,
 C Alveolar Lateralfricative Voiced,
 C Alveolar Approximant Voiced,
 C Alveolar Lateralapproximant Voiced,
 C Postalveolar Fricative Voiceless,
 C Postalveolar Fricative Voiced,
 C Retroflex Plosive Voiceless,
 C Retroflex Plosive Voiced,
 C Retroflex Nasal Voiceless,
 C Retroflex Tap Voiceless,
 C Retroflex Fricative Voiceless,
 C Retroflex Fricative Voiced,
 C Retroflex Approximant Voiced,
 C Retroflex Lateralapproximant Voiced,
 C Palatal Plosive Voiceless,
 C Palatal Plosive Voiced,
 C Palatal Nasal Voiced,
 C Palatal Fricative Voiceless,
 C Palatal Fricative Voiced,
 C Palatal Approximant Voiced,
 C Palatal Lateralapproximant Voiced,
 C Velar Plosive Voiceless,
 C Velar Plosive Voiced,
 C Velar Nasal Voiced,
 C Velar Fricative Voiceless,
 C Velar Fricative Voiced,
 C Velar Approximant Voiced,
 C Velar Lateralapproximant Voiced,
 C Uvular Plosive Voiceless,
 C Uvular Plosive Voiced,
 C Uvular Nasal Voiced,
 C Uvular Trill Voiced,
 C Uvular Fricative Voiceless,
 C Uvular Fricative Voiced,
 C Pharyngeal Fricative Voiceless,
 C Pharyngeal Fricative Voiced,
 C Glottal Plosive Voiceless,
 C Glottal Fricative Voiceless,
 C Glottal Fricative Voiced]
