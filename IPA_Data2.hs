{-

data source for IPA

-}
module IPA_Data2

  (
  IPA(..), 
    readable, unreadableChar, containsUnreadable, --elemIndexUnreadable,

-- simple or complex sounds
   Complexity(..), 
     complexity, isComplexity,

-- types of IPA symbols
   Typing(..), 
     typing, isTyping,

-- specifications for airstreams
   Airstream(..), 
     airstreams, areAirstreams,

-- specifications for boundaries
    Boundary(..),bSym, bInventory,
      boundary, isBoundary,

-- specifications for modifiers
    Modif(..), mSym, mInventory, -- preSym, preInventory, preUniqueSym
      modifiers, containsModifier, 
        duration, tone, phonation, syllabic, tongueroot, rhotacized,

-- specifications for sounds
    Sound(..), sSym, sInventory, xSym, xInventory,
      sounds, areSounds, 
        Manner(..), 
          manner, uManner,
        Place(..), 
          place, uPlace,
        Voicing(..), 
          voicing, uVoicing,
        Frontness(..), 
          frontness, uFrontness,
        Openness(..), 
          openness, uOpenness,
        Roundedness(..), 
          roundedness, uRoundedness,

-- ejectivity marker and ties   
  ejm, tieSym, 

-- other functions
  tuplify, allTheSame,

  )

where

import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as MapL
import Data.List

-- a metatype for IPA symbols
class ShowIPA a where
  showIPA :: a -> [Char]

data IPA = IPA [((Sound,Airstream), [Modif])] | IPAb Boundary | IPAm Modif | IPAs (Sound,Airstream) | Unreadable Char
  deriving (Eq, Read) 

instance Show IPA where
  show (IPA i@(x:xs)) | xs == [] = showIPA $ IPA [x]
                      | otherwise = showL "Complex Sound: (" ", " ")" "" $ (map IPA $ map (:[]) i)
  show (IPAb boundary) = show boundary
  show (IPAm modif) = show modif
  show (IPAs (sound, airstream)) = showIPA sound ++ showIPA airstream
  show (Unreadable char) = "Unreadable " ++ show char

-- i did this to avoid flexible instances (or typesynonym instances) to keep it portable
instance ShowIPA IPA where
  showIPA (IPA [((sound,airstream),modifs)]) = (showL "" ", " " " "" modifs)++showIPA sound++showIPA airstream


readable :: IPA -> Bool
readable (Unreadable _) = False
readable _ = True

unreadableChar :: IPA -> Maybe Char
unreadableChar (Unreadable c) = Just c
unreadableChar _ = Nothing

containsUnreadable :: [IPA] -> Bool
containsUnreadable ipal = foldl (==) True $ map readable ipal

{-}
elemIndexUnreadable :: [IPA] -> [Int]
elemIndexUnreadable ipal = foldlTrace (+) 0 [] $ jumperate 0 $ map readable ipal 

jumperate n [] = []
jumperate n b = case elemIndex False b of
          Just i  -> [i+n+1] ++ jumperate i (drop (i+1) b)
          Nothing -> []


--foldlTrace :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlTrace f v temp [] = temp
foldlTrace f v temp (x:xs) = foldlTrace f next (temp++[next]) xs
  where next = f v x
-}
-- differentiation between simple and compound sounds data structure and accessing functions
data Complexity = Simplex | Complex
  deriving (Eq, Show, Read)

complexity :: IPA -> Complexity
complexity ipa = case length $ sounds ipa of
  1 -> Simplex
  0 -> error "empty sound"
  _ -> Complex

isComplexity :: Complexity -> IPA -> Bool
isComplexity c ipa = (==) c $ complexity ipa



-- sound typing data structure and accessing functions
-- in this i assume that there are only complex sounds of the same type. 
data Typing = Consonant | Vowel | Diacritic | Boundary | Sound | None
  deriving (Eq, Show, Read)

typing :: IPA -> Typing
typing (IPAb _) = Boundary
typing (IPAm _) = Diacritic
typing (IPAs _) = Sound
typing (Unreadable _) = None
typing ipa = if allTheSame types then head types
  else error "mixed typed complex sound"
  where typing2 (C _ _ _) = Consonant
        typing2 (V _ _ _) = Vowel
        types = map typing2 $ sounds ipa 

isTyping :: Typing -> IPA -> Bool
isTyping t ipa = (==) t $ typing ipa





-- sound airstream data structure and accessing functions
data Airstream = PulmonicEgressive | GlottalicEgressive | GlottalicIngressive | LingualIngressive
  deriving (Eq, Show, Read)

instance ShowIPA Airstream where
  showIPA airstream = if airstream == GlottalicEgressive then " Ejective" else ""


airstreams :: IPA -> [Airstream]
airstreams (IPA s) = getAirstreams s
  where getAirstreams [] = []
        getAirstreams (((_,a),_):xs) = [a] ++ getAirstreams xs
airstreams _ = error "call for airstreams to non-sound ipa"

areAirstreams :: Airstream -> IPA -> [Bool]
areAirstreams air ipa = map ((==) air) $ airstreams ipa





-- creates tuples out of lists, safe version of zip
tuplify :: [a] -> [b] -> [(a, b)]
tuplify [] [] = []
tuplify (x:xs) [] = error "mismatched tuples"
tuplify [] (y:ys) = error "mismatched tuples"
tuplify (x:xs) (y:ys) = [(x,y)] ++ tuplify xs ys

-- checks if all elements of a list are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- converts a list into a string, there is a showList which i am ignoring
showL :: (Eq a, Show a) => [Char] -> [Char] -> [Char] -> [Char] -> [a] -> [Char]
showL init inter term alt [] = alt
showL init inter term alt (x:xs) = init ++ show x ++ showL2 xs
  where showL2 [] = term
        showL2 (x:xs) = inter++show x++showL2 xs

-- ejectivity marker
ejm = 'ʼ'





-- boundaries
data Boundary = MajorGroup | MinorGroup | GlobalRise | GlobalFall | Whitespace | Undertie | 
 SyllableBoundary | PrimaryStress | SecondaryStress 
  deriving (Eq, Show, Read, Ord)

boundary :: IPA -> Boundary
boundary (IPAb b) = b
boundary _ = error "call for boundary type to non-boundary ipa"

isBoundary :: IPA -> Boundary -> Bool
isBoundary ipa b = (==) b $ boundary ipa


bInventory = Map.fromList bSource

bSource = tuplify bSym bSpec

bSym = [' ', '.', '\8255', '\712', '\716', '|', '\8214', '\8599', '\8600']

bSpec = [IPAb Whitespace,
 IPAb SyllableBoundary,
 IPAb Undertie,
 IPAb PrimaryStress,
 IPAb SecondaryStress,
 IPAb MinorGroup,
 IPAb MajorGroup,
 IPAb GlobalRise,
 IPAb GlobalFall]





-- modifiers: diacritics, suprasegmentals, tone
data Modif = Aspirated | Labialized | Palatalized | Velarized | Pharyngealized | NasalReleased |
 LateralReleased | Long | HalfLong | HighTone | MidTone | LowTone | Devoiced | Envoiced |
 MoreRounded | LessRounded | Advanced | Retracted | Centralized | MidCentralized | Syllabic |
 NonSyllabic | Rhotacized | BreathyVoiced | CreakyVoiced | LinguoLabial |
 VelarizedOrPharyngealized | Raised | Lowered | AdvancedTongueRoot | RetractedTongueRoot |
 Dentalized | Apical | Laminal | Nasalized | Unreleased | ExtraHighTone | ExtraLowTone |
 FallingTone | RisingTone | HighRisingTone | LowRisingTone | RisingFallingTone | ExtraShort |
 EjectivityMarker | NormalVoiced | NoTone | Short | Tiebar | MurmuredVoiced | NonRhotacized |
 BilabialVoicedPrestopped | AlveolarVoicedPrestopped | VelarVoicedPrestopped | BilabialPrenasalized | 
 AlveolarPrenasalized | VelarPrenasalized | RetroflexPrenasalized | PalatalPrenasalized | 
 PalatalVoicedPrestopped | BilabialVoicedPoststopped | AlveolarVoicedPoststopped |
 VelarVoicedPoststopped | PalatalVoicedPoststopped | BilabialVoicelessPoststopped | 
 ModifTiebar | Upstep | Downstep | UnreadableModif
  deriving (Eq, Show, Read)


mInventory = Map.fromList $ tuplify mSym mSpec
preInventory = Map.fromList $ tuplify preSym preSpec

modifiers :: IPA -> [[Modif]]
modifiers (IPA s) = getModifs s
  where getModifs [] = []
        getModifs ((_,m):xs) = [m] ++ getModifs xs
modifiers _ = error "call for modifiers to non-sound ipa"

containsModifier :: Modif -> IPA -> [Bool]
containsModifier modif ipa = map (elem modif) $ modifiers ipa


mSym = diaSym++supSym++tonSym++tieSym
mSpec = diaSpec++supSpec++tonSpec++tieSpec

-- not in use
preSym = ['ᵇ', 'ᵈ', 'ᶢ', 'ᶡ', 'ᵐ', 'ⁿ', 'ᵑ', 'ᶮ', 'ᶯ']

preUniqueSym = ['ꜜ', 'ꜝ', 'ꜛ', 'ꜞ', 'ꜟ']

diaSym = ['\805', '\812', 'ʰ', '\825', '\796', '\799', '\800', '\776', '\829', '\809',
  '\815', '\734', '\804', '\816', '\828', 'ʷ', 'ʲ', 'ˠ', 'ˤ', '\820', '\797', '\798',
  '\792', '\793', '\810', '\826', '\827', '\771', 'ⁿ', 'ˡ', '\794', '\781', 'ʱ', 'ʴ', '\778',
  'ᵇ', 'ᵈ', 'ᶢ', 'ᶡ', 'ᵖ']

supSym = ['ː','ˑ','\774']

tonSym = ['˥','˦','˧','˨','˩', '\779', '\769', '\772', '\768', '\783', '\780', '\770', 
  '\7620', '\7621', '\7624']

tieSym = ['\865','\860']
tieSpec = [Tiebar, Tiebar]
-- for pre-effects parser (bad idea, produces ambiguity)
tieOther = [ModifTiebar]

-- not in use
preSpec = [BilabialVoicedPrestopped,
 AlveolarVoicedPrestopped,
 VelarVoicedPrestopped,
 PalatalVoicedPrestopped,
 BilabialPrenasalized,
 AlveolarPrenasalized,
 VelarPrenasalized,
 PalatalPrenasalized,
 RetroflexPrenasalized]

preUniqueSpec = [Downstep,
 Downstep,
 Upstep,
 Upstep,
 Upstep]

diaSpec = [Devoiced,
 Envoiced,
 Aspirated,
 MoreRounded,
 LessRounded,
 Advanced,
 Retracted,
 Centralized,
 MidCentralized,
 Syllabic,
 NonSyllabic,
 Rhotacized,
 BreathyVoiced,
 CreakyVoiced,
 LinguoLabial,
 Labialized,
 Palatalized,
 Velarized,
 Pharyngealized,
 VelarizedOrPharyngealized,
 Raised,
 Lowered,
 AdvancedTongueRoot,
 RetractedTongueRoot,
 Dentalized,
 Apical,
 Laminal,
 Nasalized,
 NasalReleased,
 LateralReleased,
 Unreleased,
 Syllabic,
 MurmuredVoiced,
 Rhotacized,
 Devoiced,
 BilabialVoicedPoststopped,
 AlveolarVoicedPoststopped,
 VelarVoicedPoststopped,
 PalatalVoicedPoststopped,
 BilabialVoicelessPoststopped]

supSpec = [Long,
 HalfLong,
 ExtraShort]

tonSpec = [ExtraHighTone,
 HighTone,
 MidTone,
 LowTone,
 ExtraLowTone,
 ExtraHighTone,
 HighTone,
 MidTone,
 LowTone,
 ExtraLowTone,
 RisingTone,
 FallingTone,
 HighRisingTone,
 LowRisingTone,
 RisingFallingTone]


tonguerootM = [AdvancedTongueRoot, RetractedTongueRoot]
tongueroot :: IPA -> [Modif]
tongueroot ipa = map eval $ map (intersect tonguerootM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> RetractedTongueRoot
          _ -> error "too many ATR specifications applied"


durationM = [Long, HalfLong, ExtraShort, Short]
duration :: IPA -> [Modif]
duration ipa = map eval $ map (intersect durationM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> Short
          _ -> error "too many length-suprasegmentals applied"


toneM = [HighTone, MidTone, LowTone, ExtraHighTone, ExtraLowTone, FallingTone, RisingTone, HighRisingTone, LowRisingTone, RisingFallingTone, NoTone]
tone :: IPA -> [Modif]
tone ipa = map eval $ map (intersect toneM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> NoTone
          _ -> error "too many tone-suprasegmentals applied"


phonationM = [BreathyVoiced, CreakyVoiced, MurmuredVoiced, NormalVoiced]
phonation :: IPA -> [Modif]
phonation ipa = map eval $ map (intersect phonationM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> NormalVoiced
          _ -> error "too many modifiers for phonation"


syllabicM = [Syllabic, NonSyllabic]
syllabic :: IPA -> [Modif]
syllabic ipa = map eval $ map (intersect syllabicM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> deflt
          _ -> error "too many modifiers for syllabicity"
        deflt = if Vowel `isTyping` ipa then Syllabic else NonSyllabic

rhotacizedM = [Rhotacized, NonRhotacized]
rhotacized :: IPA -> [Modif]
rhotacized ipa = map eval $ map (intersect syllabicM) $ modifiers ipa
  where eval x = case length x of
          1 -> head x
          0 -> NonRhotacized
          _ -> error "too many modifiers for rhotacity"



placeM = [Labialized, Palatalized, Velarized, Pharyngealized, LinguoLabial, VelarizedOrPharyngealized, Dentalized, Laminal, Apical, Advanced, Retracted, Lowered, Raised]
place :: IPA -> [(Place,[Modif])]
place ipa = tuplify (uPlace ipa) (map (intersect placeM) $ modifiers ipa)


mannerM = [NasalReleased, LateralReleased, Rhotacized, Nasalized, Unreleased, Aspirated]
manner :: IPA -> [(Manner,[Modif])]
manner ipa = tuplify (uManner ipa) (map (intersect mannerM) $ modifiers ipa)


voicingM = [Devoiced, Envoiced]
voicing :: IPA -> [(Voicing,[Modif])]
voicing ipa = tuplify (uVoicing ipa) (map (intersect voicingM) $ modifiers ipa)


opennessM = [MidCentralized, Raised, Lowered]
openness :: IPA -> [(Openness,[Modif])]
openness ipa = tuplify (uOpenness ipa) (map (intersect opennessM) $ modifiers ipa)


frontnessM = [Centralized, MidCentralized]
frontness :: IPA -> [(Frontness,[Modif])]
frontness ipa = tuplify (uFrontness ipa) (map (intersect frontnessM) $ modifiers ipa)


roundednessM = [MoreRounded, LessRounded]
roundedness :: IPA -> [(Roundedness,[Modif])]
roundedness ipa = tuplify (uRoundedness ipa) (map (intersect roundednessM) $ modifiers ipa)





--sounds
data Sound = C Place Manner Voicing | V Openness Frontness Roundedness | UnreadableSound
  deriving (Eq, Show, Read)

instance ShowIPA Sound where
  showIPA (C p m v) = show v++" "++show p++" "++show m
  showIPA (V o f r) = show o++" "++show f++" "++show r++" Vowel"
  showIPA UnreadableSound = "unreadable sound"


sounds :: IPA -> [Sound]
sounds (IPA s) = getSounds s
  where getSounds [] = []
        getSounds (((s,_),_):xs) = [s] ++ getSounds xs
sounds _ = error "call for sounds to non-sound ipa"

areSounds :: Sound -> IPA -> [Bool]
areSounds snd ipa = map ((==) snd) $ sounds ipa


xInventory = Map.fromList xSource

xSource = tuplify xSym $ flip tuplify xDia xSpec 

xSym = ['ɚ', 'ɝ']

xSpec = [(V Mid Central Unrounded, PulmonicEgressive), (V OpenMid Central Unrounded, PulmonicEgressive)]

xDia = [[Rhotacized], [Rhotacized]]


sInventory = Map.fromList soundSource

soundSource = lingualingressive++glottalicingressive++pulmonicegressive
lingualingressive = tuplify liSym $ tuplify liSpec $ replicate (length liSpec) LingualIngressive
glottalicingressive = tuplify giSym $ tuplify giSpec $ replicate (length giSpec) GlottalicIngressive
pulmonicegressive = tuplify peSym $ tuplify peSpec $ replicate (length peSpec) PulmonicEgressive

sSym = peSym++giSym++liSym
peSym = cSym++vSym
peSpec = cSpec++vSpec

vSym = ['i','y','ɨ','ʉ','ɯ','u','ɪ','ʏ','ʊ','e','ø','ɘ','ɵ','ɤ','o','ə',
  'ɛ','œ','ɜ','ɞ','ʌ','ɔ','æ','ɐ','a','ɶ','ɑ','ɒ']

cSym = ['p','b','m','ʙ','ɸ','β','ɱ','v','f','ⱱ','ʋ','θ','ð','t','d','n','r','ɾ',
  's','z','ɬ','ɮ','ɹ','l','ʃ','ʒ','ʈ','ɖ','ɳ','ɽ','ʂ','ʐ','ɻ','ɭ','c','ɟ','ɲ','ç','ʝ',
  'j','ʎ','k','g','ŋ','x','ɣ','ɰ','ʟ','q','ɢ','ɴ','ʀ','χ','ʁ','ħ','ʕ','ʔ','h','ɦ',
  'ʍ','w','ɥ','ʜ','ʢ','ɕ','ʑ','ɺ','ʡ','ɡ']

giSym = ['ɓ','ɗ','ʄ','ɠ','ʛ']

liSym = ['ʘ','ǃ','ǂ','ǁ','ǀ']

data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Retroflex | Palatal | Velar | Uvular | Pharyngeal | Glottal | LabialVelar | LabialPalatal | Epiglottal | AlveoloPalatal | PalatoAlveolar
  deriving (Eq, Show, Read)

data Manner = Plosive | Nasal | Trill | Tap | Fricative | LateralFricative | Approximant | Lateralapproximant | LateralFlap | Click | LateralClick | Implosive
  deriving (Eq, Show, Read)

data Voicing = Voiced | Voiceless
  deriving (Eq, Show, Read)

data Openness = Close | CloseCloseMid | CloseMid | Mid | OpenMid | OpenOpenMid | Open
  deriving (Eq, Show, Read, Ord)

data Frontness = Front | FrontCentral | Central | BackCentral | Back
  deriving (Eq, Show, Read, Ord)

data Roundedness = Rounded | Unrounded
  deriving (Eq, Show, Read)


uPlace :: IPA -> [Place]
uPlace ipa = map place $ sounds ipa
  where place (C place _ _) = place

uManner :: IPA -> [Manner]
uManner ipa = map manner $ sounds ipa
  where manner (C _ manner _) = manner

uVoicing :: IPA -> [Voicing]
uVoicing ipa = map voicing $ sounds ipa
  where voicing (C _ _ voicing) = voicing

uOpenness :: IPA -> [Openness]
uOpenness ipa = map open $ sounds ipa
  where open (V open _ _) = open

uFrontness :: IPA -> [Frontness]
uFrontness ipa = map front $ sounds ipa
  where front (V _ front _) = front

uRoundedness :: IPA -> [Roundedness]
uRoundedness ipa = map rounded $ sounds ipa
  where rounded (V _ _ rounded) = rounded

giSpec = [ C Bilabial Implosive Voiced,
 C Alveolar Implosive Voiced,
 C Palatal Implosive Voiced,
 C Velar Implosive Voiced,
 C Uvular Implosive Voiced]

liSpec = [ C Bilabial Click Voiceless,
 C Alveolar Click Voiceless,
 C PalatoAlveolar Click Voiceless,
 C Alveolar LateralClick Voiceless,
 C Dental Click Voiceless]

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
 C Alveolar LateralFricative Voiceless,
 C Alveolar LateralFricative Voiced,
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
 C Glottal Fricative Voiced,
 C LabialVelar Fricative Voiceless,
 C LabialVelar Approximant Voiced,
 C LabialPalatal Approximant Voiced,
 C Epiglottal Fricative Voiceless,
 C Epiglottal Fricative Voiced,
 C AlveoloPalatal Fricative Voiceless,
 C AlveoloPalatal Fricative Voiced,
 C Alveolar LateralFlap Voiced,
 C Epiglottal Plosive Voiceless,
 C Velar Plosive Voiced]