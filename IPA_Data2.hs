{-

idea: 
downgrade xSpec lists from type IPA to their respective types and create xInventory via function

-}
module IPA_Data2
  (
  IPA(..),
  Property(..), T(..),
-- vowels and consonants
  iInventory, iSym, iSpec,

-- whitespaces, boundaries
  Boundary(..), bSym, bSpec, bInventory,

-- metatype modifiers for diacritics, suprasegmentals & tone
  Modif(..),  mInventory, mSym, mSpec,

  Category(..),
  Place, Manner, Voicing,
  Openness, Frontness, Roundedness
  )

where

import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as MapL

-- a metatype for IPA symbols
data IPA = IPA T Property Property Property | IPAb Boundary | IPAm Modif | Unreadable
  deriving (Eq, Show, Read)

-- for inferring Consonant or Vowel status
data T = C | V
  deriving (Eq, Show, Read)

-- all possible C or V properties
data Property = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Retroflex | Palatal | Velar | Uvular | Pharyngeal | Glottal | Close | CloseCloseMid | CloseMid | Mid | OpenMid | OpenOpenMid | Open | Plosive | Nasal | Trill | Tap | Fricative | Lateralfricative | Approximant | Lateralapproximant | Front | FrontCentral | Central | BackCentral | Back | Voiced | Voiceless | Rounded | Unrounded
  deriving (Eq, Show, Read)

-- type synonyms for feat function
type Place = Property
type Manner = Property
type Voicing = Property

type Openness = Property
type Frontness = Property
type Roundedness = Property

-- a metatype for all feature categories
data Category = Place | Manner | Voicing | Openness | Frontness | Roundedness
  deriving (Eq, Show, Read)

-- diacritics, so far only diacritics for superscript symbols
-- basic suprasegmentals and tone TONE IS IMPLEMENTED OUTSIDE IPA STANDARD! extra high -> falling, extra low -> rising
data Modif = Aspirated | Labialized | Palatalized | Velarized | Pharyngealized | NasalRelease | LateralRelease | Long | HalfLong | FallingTone | HighTone | MidTone | LowTone | RisingTone
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
diaSpec = [IPAm Aspirated, IPAm Labialized, IPAm Palatalized, IPAm Velarized, IPAm Pharyngealized, IPAm NasalRelease, IPAm LateralRelease]

supSym = ['ː','ˑ']
supSpec = [IPAm Long, IPAm HalfLong]

tonSym = ['˥','˦','˧','˨','˩']
tonSpec = [IPAm FallingTone, IPAm HighTone, IPAm MidTone, IPAm LowTone, IPAm RisingTone]

iSym = cSym++vSym
iSpec = cSpec++vSpec

vSym = ['i','y','ɨ','ʉ','ɯ','u','ɪ','ʏ','ʊ','e','ø','ɘ','ɵ','ɤ','o','ə',
  'ɛ','œ','ɜ','ɞ','ʌ','ɔ','æ','ɐ','a','ɶ','ɑ','ɒ']

vSpec = [IPA V Close Front Unrounded,
 IPA V Close Front Rounded,
 IPA V Close Central Unrounded,
 IPA V Close Central Rounded,
 IPA V Close Back Unrounded,
 IPA V Close Back Rounded,
 IPA V CloseCloseMid FrontCentral Unrounded,
 IPA V CloseCloseMid FrontCentral Rounded,
 IPA V CloseCloseMid BackCentral Rounded,
 IPA V CloseMid Front Unrounded,
 IPA V CloseMid Front Rounded,
 IPA V CloseMid Central Unrounded,
 IPA V CloseMid Central Rounded,
 IPA V CloseMid Back Unrounded,
 IPA V CloseMid Back Rounded,
 IPA V Mid Central Unrounded,
 IPA V OpenMid Front Unrounded,
 IPA V OpenMid Front Rounded,
 IPA V OpenMid Central Unrounded,
 IPA V OpenMid Central Rounded,
 IPA V OpenMid Back Unrounded,
 IPA V OpenMid Back Rounded,
 IPA V OpenOpenMid Front Unrounded,
 IPA V OpenOpenMid Central Unrounded,
 IPA V Open Front Unrounded,
 IPA V Open Front Rounded,
 IPA V Open Back Unrounded,
 IPA V Open Back Rounded]

cSym = ['p','b','m','ʙ','ɸ','β','ɱ','ⱱ','f','v','ʋ','θ','ð','t','d','n','r','ɾ',
  's','z','ɬ','ɮ','ɹ','l','ʃ','ʒ','ʈ','ɖ','ɳ','ɽ','ʂ','ʐ','ɻ','ɭ','c','ɟ','ɲ','ç','ʝ',
  'j','ʎ','k','g','ŋ','x','ɣ','ɰ','ʟ','q','ɢ','ɴ','ʀ','χ','ʁ','ħ','ʕ','ʔ','h','ɦ']

cSpec = [IPA C Bilabial Plosive Voiceless,
 IPA C Bilabial Plosive Voiced,
 IPA C Bilabial Nasal Voiced,
 IPA C Bilabial Trill Voiced,
 IPA C Bilabial Fricative Voiceless,
 IPA C Bilabial Fricative Voiced,
 IPA C Labiodental Nasal Voiced,
 IPA C Labiodental Fricative Voiced,
 IPA C Labiodental Fricative Voiceless,
 IPA C Labiodental Tap Voiced,
 IPA C Labiodental Approximant Voiced,
 IPA C Dental Fricative Voiceless,
 IPA C Dental Fricative Voiced,
 IPA C Alveolar Plosive Voiceless,
 IPA C Alveolar Plosive Voiced,
 IPA C Alveolar Nasal Voiced,
 IPA C Alveolar Trill Voiced,
 IPA C Alveolar Tap Voiced,
 IPA C Alveolar Fricative Voiceless,
 IPA C Alveolar Fricative Voiced,
 IPA C Alveolar Lateralfricative Voiceless,
 IPA C Alveolar Lateralfricative Voiced,
 IPA C Alveolar Approximant Voiced,
 IPA C Alveolar Lateralapproximant Voiced,
 IPA C Postalveolar Fricative Voiceless,
 IPA C Postalveolar Fricative Voiced,
 IPA C Retroflex Plosive Voiceless,
 IPA C Retroflex Plosive Voiced,
 IPA C Retroflex Nasal Voiceless,
 IPA C Retroflex Tap Voiceless,
 IPA C Retroflex Fricative Voiceless,
 IPA C Retroflex Fricative Voiced,
 IPA C Retroflex Approximant Voiced,
 IPA C Retroflex Lateralapproximant Voiced,
 IPA C Palatal Plosive Voiceless,
 IPA C Palatal Plosive Voiced,
 IPA C Palatal Nasal Voiced,
 IPA C Palatal Fricative Voiceless,
 IPA C Palatal Fricative Voiced,
 IPA C Palatal Approximant Voiced,
 IPA C Palatal Lateralapproximant Voiced,
 IPA C Velar Plosive Voiceless,
 IPA C Velar Plosive Voiced,
 IPA C Velar Nasal Voiced,
 IPA C Velar Fricative Voiceless,
 IPA C Velar Fricative Voiced,
 IPA C Velar Approximant Voiced,
 IPA C Velar Lateralapproximant Voiced,
 IPA C Uvular Plosive Voiceless,
 IPA C Uvular Plosive Voiced,
 IPA C Uvular Nasal Voiced,
 IPA C Uvular Trill Voiced,
 IPA C Uvular Fricative Voiceless,
 IPA C Uvular Fricative Voiced,
 IPA C Pharyngeal Fricative Voiceless,
 IPA C Pharyngeal Fricative Voiced,
 IPA C Glottal Plosive Voiceless,
 IPA C Glottal Fricative Voiceless,
 IPA C Glottal Fricative Voiced]
