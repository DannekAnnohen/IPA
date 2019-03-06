module IPA_data
  (
  IPA(..), ipaSym,
    readable, unreadableChar, properIPA, satisfy, elemIndexSatisfy,
    showIPA,

-- simple or complex sounds
   Complexity(..), 
     complexity, isComplexity, splitIPA,
     assign,

-- types of IPA symbols
   Typing(..), 
     typing, isTyping,

-- specifications for airstreams
   Airstream(..), 
     airstream, isAirstream, complexAirstream,

-- specifications for boundaries
    Boundary(..),bSym, bInventory, bInventory_inv,
      boundary, isBoundary,

-- specifications for modifiers
    Modif(..), mSym, ejm, tieSym, mInventory, mInventory_inv, -- not in use: preSym, preInventory, preUniqueSym
      modifiers, isModifier, containsModifier, complexModifiers,
        duration, tone, phonation, syllabic, tongueroot, rhotacized,
      -- functions joining underlying features and modifiers
      SurfaceFeat(..),
        modified, complexSurfaceFeat,
          showModif,
          place, showPlace,
          manner, showManner,
          voicing, showVoicing, --works for consonants and vowels
          openness, showOpenness,
          frontness, showFrontness,
          roundedness, showRoundedness,

-- specifications for sounds
    Sound(..),sSym, sInventory, sInventory_inv, xSym, xSpec, xInventory, xInventory_inv,
      sound, isSound, complexSound,
        Place(..), 
          uPlace, complexPlace,
        Manner(..), 
          uManner, complexManner,
        Voicing(..), 
          uVoicing, complexVoicing, --works for consonants and vowels
        Openness(..), 
          uOpenness, complexOpenness,
        Frontness(..), 
          uFrontness, complexFrontness,
        Roundedness(..), 
          uRoundedness, complexRoundedness,
  )
where

import qualified Data.Map.Lazy as Map
import Data.List
import Data.Char
import IPA_util


-- deriving Ord here and in all other constituent data declarations for implmentation of Maps (this leaves room for improvement, motivated for openness/frontness, but rest?)
data IPA = IPA [((Sound,Airstream), [Modif])] | IPAb Boundary | IPAs ((Sound,Airstream),[Modif]) | IPAm Modif | Unreadable Char
  deriving (Eq, Read, Ord) 

-- collection of all readable IPA-symbols
ipaSym = [ejm]++sSym++xSym++mSym++bSym

readable :: IPA -> Bool
readable (Unreadable _) = False
readable _ = True

properIPA :: IPA -> Bool
properIPA (IPA _) = True
properIPA (IPAb _) = True
properIPA _ = False

unreadableChar :: IPA -> Maybe Char
unreadableChar (Unreadable c) = Just c
unreadableChar _ = Nothing

{- Copy from IPA_util
satisfy :: Foldable t => (a -> Bool) -> t a -> Bool
satisfy f ipal = not $ foldl (flip ((==) . f)) False ipal

elemIndexSatisfy :: (a -> Bool) -> [a] -> [Int]
elemIndexSatisfy f ipal = jumperate 0 $ map f ipal 
  where jumperate _ [] = []
        jumperate n b = case elemIndex False b of
          Just i  -> [n+i] ++ jumperate (i+n+1) (drop (i+1) b)
          Nothing -> []
-}

class ShowIPA a where
  showIPA :: a -> [Char]

instance ShowIPA IPA where
  showIPA ipa@(IPA (((sound,airstream),modifs):xs)) | xs == [] = (showL showIPA "" ", " " " "" modifs')++pres sound
                                   | otherwise = showL show "Complex Sound: (" ", " ")" "" $ splitIPA ipa
    where 
      modifs' = (\\) modifs $ (lateralM ++ voicingM ++ toneM) 
      pres (C p m v) = (if modified $ voicing ipa then showIPA $ voicing ipa else show v)++" "++showIPA p++" "++(if Lateral `elem` modifs then "Lateral " else "")++ (if airstream == GlottalicEgressive then "Ejective " ++ showIPA m else showIPA m ++ showIPA airstream)
      pres (V o f r) = (if modified $ voicing ipa then (showIPA $ voicing ipa) ++ " " else "")++showIPA o++" "++showIPA f++" "++showIPA r++" Vowel"++(if tone ipa /= NoTone then ", " ++ (showIPA $ tone ipa) else "")
  showIPA (IPAb boundary) = showIPA boundary
  showIPA (IPAm modif) = showIPA modif
  showIPA (IPAs ((sound, airstream),modif)) = "IPAs (("++showIPA sound ++", "++ show airstream++"), "++show modif++")"
  showIPA (Unreadable char) = "Unreadable " ++ show char


instance Show IPA where
  show ipa@(IPA (((sound,airstream),modifs):xs)) | xs == [] = (showL show "" ", " " " "" modifs)++pres sound ++(if airstream == GlottalicEgressive then " Ejective" else "")
                                                 | otherwise = showL show "Complex Sound: (" ", " ")" "" $ splitIPA ipa
    where 
      pres (C p m v) = show v++" "++show p++" "++ show m
      pres (V o f r) = show o++" "++show f++" "++show r++" Vowel"
  show (IPAb boundary) = "IPAb "++show boundary
  show (IPAm modif) = "IPAm "++show modif
  show (IPAs ((sound, airstream),modif)) = "IPAs (("++show sound ++", "++ show airstream++"), "++show modif++")"
  show (Unreadable char) = "Unreadable " ++ show char

-- sound typing data structure and accessing functions
data Typing = ComplexV | ComplexC | Consonant | Vowel | Diacritic | Boundary | RawSound | None | MixedComplex
  deriving (Eq, Show, Read)

typing :: IPA -> Typing
typing (IPAb _) = Boundary
typing (IPAm _) = Diacritic
typing (IPAs _) = RawSound
typing (Unreadable _) = None
typing ipa@(IPA (((s,a),m):xs)) = if xs == [] 
  then typing2 s else if allTheSame types then case head types of
  Consonant -> ComplexC
  Vowel -> ComplexV
  else MixedComplex
  where typing2 (C _ _ _) = Consonant
        typing2 (V _ _ _) = Vowel
        types = map typing2 $ complexSound $ sound ipa 

isTyping :: IPA -> Typing -> Bool
isTyping ipa t = (==) t $ typing ipa



-- differentiation between simple and compound sounds data structure and accessing functions
data Complexity = Simplex | Complex
  deriving (Eq, Show, Read)

complexity :: IPA -> Complexity
complexity (IPA ipa) = case length ipa of
  1 -> Simplex
  0 -> error "empty sound"
  _ -> Complex

isComplexity :: IPA -> Complexity -> Bool
isComplexity ipa c = (==) c $ complexity ipa

splitIPA :: IPA -> [IPA]
splitIPA (IPA []) = []
splitIPA (IPA (i:is)) = [IPA [i]] ++ splitIPA (IPA is)


-- boundaries
data Boundary = SyllableBoundary | Whitespace | Undertie | PrimaryStress |
  SecondaryStress | MinorGroup | MajorGroup | GlobalRise | GlobalFall
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Boundary where
  showIPA = show

boundary :: IPA -> Boundary
boundary (IPAb b) = b
boundary _ = error "call to function 'boundary' for non-boundary ipa"

isBoundary :: IPA -> Boundary -> Bool
isBoundary ipa b = (==) b $ boundary ipa

bInventory = Map.fromList $ tuplify bSym bSpec
bInventory_inv = Map.fromList $ (flip tuplify) bSym bSpec

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



-- sound airstream data structure and accessing functions
data Airstream = PulmonicEgressive | GlottalicEgressive | GlottalicIngressive | LingualIngressive | Air [Airstream]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Airstream where
  showIPA GlottalicEgressive = "Ejective"
  showIPA GlottalicIngressive = "Implosive"
  showIPA LingualIngressive = "Click"
  showIPA PulmonicEgressive = ""

airstream :: IPA -> Airstream
airstream (IPA s) = case length $ air of
  0 -> error "call to function 'airstream' for empty sound"
  1 -> head air
  _ -> Air air
  where getAirstreams [] = []
        getAirstreams (((_,a),_):xs) = [a] ++ getAirstreams xs
        air = getAirstreams s
airstream _ = error "call to function 'airstream' for non-sound ipa"

isAirstream :: IPA -> Airstream -> Bool
isAirstream ipa air = (==) air $ airstream ipa

-- intentionally not defined for simple modifierlists
complexAirstream :: Airstream -> [Airstream]
complexAirstream (Air xs) = xs



-- modifiers: diacritics, suprasegmentals, tone
data Modif = Aspirated | Labialized | Palatalized | Velarized | Pharyngealized | NasalReleased |
 Lateral | NonLateral | Long | HalfLong | HighTone | MidTone | LowTone | Devoiced | Envoiced |
 MoreRounded | LessRounded | Advanced | Retracted | Centralized | MidCentralized | Syllabic |
 NonSyllabic | Rhotacized | BreathyVoiced | CreakyVoiced | LinguoLabial |
 VelarizedOrPharyngealized | Raised | Lowered | AdvancedTongueroot | RetractedTongueroot |
 Dentalized | Apical | Laminal | Nasalized | Unreleased | ExtraHighTone | ExtraLowTone |
 FallingTone | RisingTone | HighRisingTone | LowRisingTone | RisingFallingTone | ExtraShort |
 EjectivityMarker | NormalVoiced | NoTone | Short | Tiebar | TieEnd | MurmuredVoiced | NonRhotacized |
 BilabialVoicedPrestopped | AlveolarVoicedPrestopped | VelarVoicedPrestopped | BilabialPrenasalized | 
 AlveolarPrenasalized | VelarPrenasalized | RetroflexPrenasalized | PalatalPrenasalized | 
 PalatalVoicedPrestopped | BilabialVoicedPoststopped | AlveolarVoicedPoststopped |
 VelarVoicedPoststopped | PalatalVoicedPoststopped | BilabialVoicelessPoststopped | 
 Upstep | Downstep | Modif [Modif]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Modif where
  showIPA VelarizedOrPharyngealized = "Velarized/Pharyngealized"
  showIPA Tiebar = show Tiebar
  showIPA TieEnd = show TieEnd
  showIPA EjectivityMarker = show EjectivityMarker
  showIPA modif = tail $ concat $ intersperse "-" $ splitConserve isUpper $ show modif

-- very unhappy with output for complx modifiers [Modif [m1], Modif [m2], ..], but it seems to be the least bad way
-- i really have to keep the recursive type invisible
-- f modifiers should be invisible
modifiers :: IPA -> [Modif]
modifiers (IPA s) = case length $ m of
  1 -> head m
  0 -> error "call to function 'modifiers' for empty sound"
  _ -> map Modif m
  where getModifs [] = []
        getModifs ((_,m):xs) = [m] ++ getModifs xs
        m = getModifs s
modifiers _ = error "call to function 'modifiers' for non-sound ipa"

isModifier :: IPA -> [Modif] -> Bool
isModifier ipa modif = (==) modif $ modifiers ipa

containsModifier :: IPA -> Modif -> Bool
containsModifier ipa modif = assign (foldl (||) False) (elem modif) id complexModifiers modifiers ipa

complexModifiers :: [Modif] -> [[Modif]]
complexModifiers [] = []
complexModifiers ((Modif x):xs) = [x] ++ complexModifiers xs

complexModif :: Modif -> [Modif]
complexModif (Modif xs) = xs

-- inventories for modifiers
mInventory = Map.fromList $ tuplify mSym mSpec
mInventory_inv = Map.fromListWith (++) $ (flip tuplify) (map (replicate 1) mSym) mSpec

mSym = diaSym++supSym++tonSym++tieSym
mSpec = diaSpec++supSpec++tonSpec++tieSpec

-- assigns a value to ipa sound according to complexity
assign :: ([b1] -> b1) -> (b2 -> b1) -> (t -> b2) -> (t -> [t]) -> (IPA -> t) -> IPA -> b1
assign constr assignF filterF merger extract ipa = case complexity ipa of
  Simplex -> assignF $ filterF $ extract ipa
  Complex -> constr $ map (assignF . filterF) $ merger $ extract ipa


-- feature functions that relate to modifiers
durationD = Short
durationM = [Long, HalfLong, ExtraShort]
duration :: IPA -> Modif
duration ipa = assign Modif (defaultize durationD) (intersect durationM) complexModifiers modifiers ipa

tonguerootD = RetractedTongueroot
tonguerootM = [AdvancedTongueroot, RetractedTongueroot]
tongueroot :: IPA -> Modif
tongueroot ipa = assign Modif (defaultize tonguerootD) (intersect tonguerootM) complexModifiers modifiers ipa

toneD = NoTone
toneM = [HighTone, MidTone, LowTone, ExtraHighTone, ExtraLowTone, FallingTone, RisingTone, HighRisingTone, LowRisingTone, RisingFallingTone]
tone :: IPA -> Modif
tone ipa = assign Modif (defaultize toneD) (intersect toneM) complexModifiers modifiers ipa

phonationD = NormalVoiced
phonationM = [BreathyVoiced, CreakyVoiced, MurmuredVoiced]
phonation :: IPA -> Modif
phonation ipa = assign Modif (defaultize phonationD) (intersect phonationM) complexModifiers modifiers ipa

rhotacizedD = NonRhotacized
rhotacizedM = [Rhotacized]
rhotacized :: IPA -> Modif
rhotacized ipa = assign Modif (defaultize rhotacizedD) (intersect rhotacizedM) complexModifiers modifiers ipa

lateralD = NonLateral
lateralM = [Lateral]
lateral :: IPA -> Modif
lateral ipa = assign Modif (defaultize lateralD) (intersect lateralM) complexModifiers modifiers ipa

-- syllabicD is relative to type
syllabicM = [Syllabic, NonSyllabic]
syllabic :: IPA -> Modif
syllabic ipa = assign Modif assignF (intersect syllabicM) complexModifiers modifiers ipa
  where
    assignF xs = case length xs of
      0 -> if ipa `isTyping` Vowel then Syllabic else NonSyllabic
      1 -> head xs

uSyllabic :: IPA -> Modif
uSyllabic ipa = assign Modif syll id complexSound sound ipa
  where 
    syll (C _ _ _) = NonSyllabic
    syll (V _ _ _) = Syllabic


-- feature functions that relate to underlying features
data SurfaceFeat = Pl Place | Ma Manner | Vo Voicing | Op Openness | Fr Frontness | Ro Roundedness | Mo Modif | Com [SurfaceFeat]
  deriving (Eq, Show, Read)

instance ShowIPA SurfaceFeat where
  showIPA (Pl p) = showIPA p
  showIPA (Ma m) = showIPA m
  showIPA (Vo v) = showIPA v
  showIPA (Op o) = showIPA o
  showIPA (Fr f) = showIPA f
  showIPA (Mo m) = showIPA m
  showIPA (Com xs) = "SurfaceFeatures "++(showL showIPA "[" ", " "]" "[empty list]" xs)

modified :: SurfaceFeat -> Bool
modified (Mo _) = True
modified (Com xs) = foldl (&&) True $ map modified xs
modified _ = False

complexSurfaceFeat :: SurfaceFeat -> [SurfaceFeat]
complexSurfaceFeat (Com xs) = xs

showModif :: SurfaceFeat -> Modif
showModif (Mo m) = m
showModif _ = error "call to function 'showMod' for non-modified surface feature"

placeM = [Labialized, Palatalized, Velarized, Pharyngealized, LinguoLabial, VelarizedOrPharyngealized, Dentalized, Laminal, Apical, Advanced, Retracted, Lowered, Raised]
place :: IPA -> SurfaceFeat
place ipa = assign Com adjust (intersect placeM) complexModifiers modifiers ipa
  where adjust xs = if xs == [] then Pl $ uPlace ipa else Mo $ head xs

showPlace :: SurfaceFeat -> Place
showPlace (Pl p) = p
showPlace (Mo _) = error "call to function 'showPlace' for modified surface feature"
showPlace _ = error "call to function 'showPlace' for non-place surface feature"

mannerM = [NasalReleased, Rhotacized, Nasalized, Unreleased, Aspirated]
manner :: IPA -> SurfaceFeat
manner ipa = assign Com adjust (intersect mannerM) complexModifiers modifiers ipa
  where adjust xs = if xs == [] then Ma $ uManner ipa else Mo $ head xs

showManner :: SurfaceFeat -> Manner
showManner (Ma m) = m
showManner (Mo _) = error "call to function 'showManner' for modified surface feature"
showManner _ = error "call to function 'showManner' for non-manner surface feature"

-- works for vowels and consonants
voicingM = [Devoiced, Envoiced]
voicing :: IPA -> SurfaceFeat
voicing ipa = assign Com adjust (intersect voicingM) complexModifiers modifiers ipa
  where
    adjust xs = case typing ipa of
      Consonant -> defaultize (Vo $ uVoicing ipa) $ map Mo xs
      Vowel -> defaultize (Vo Voiced) $ map Mo xs

showVoicing :: SurfaceFeat -> Voicing
showVoicing (Vo v) = v
showVoicing (Mo _) = error "call to function 'showVoicing' for modified surface feature"
showVoicing _ = error "call to function 'showVoicing' for non-voicing surface feature"

opennessM = [MidCentralized, Raised, Lowered]
openness :: IPA -> SurfaceFeat
openness ipa = assign Com adjust (intersect opennessM) complexModifiers modifiers ipa
  where adjust xs = if xs == [] then Op $ uOpenness ipa else Mo $ head xs

showOpenness :: SurfaceFeat -> Openness
showOpenness (Op o) = o
showOpenness (Mo _) = error "call to function 'showOpenness' for modified surface feature"
showOpenness _ = error "call to function 'showOpenness' for non-openness surface feature"

frontnessM = [Centralized, MidCentralized]
frontness :: IPA -> SurfaceFeat
frontness ipa = assign Com adjust (intersect frontnessM) complexModifiers modifiers ipa
  where adjust xs = if xs == [] then Fr $ uFrontness ipa else Mo $ head xs

showFrontness :: SurfaceFeat -> Frontness
showFrontness (Fr f) = f
showFrontness (Mo _) = error "call to function 'showFrontness' for modified surface feature"
showFrontness _ = error "call to function 'showFrontness' for non-frontness surface feature"


roundednessM = [MoreRounded, LessRounded]
roundedness :: IPA -> SurfaceFeat
roundedness ipa = assign Com adjust (intersect roundednessM) complexModifiers modifiers ipa
  where adjust xs = if xs == [] then Ro $ uRoundedness ipa else Mo $ head xs

showRoundedness :: SurfaceFeat -> Roundedness
showRoundedness (Ro r) = r
showRoundedness (Mo _) = error "call to function 'showRoundedness' for modified surface feature"
showRoundedness _ = error "call to function 'showRoundedness' for non-roundedness surface feature"


-- ejectivity marker
ejm = 'ʼ'

tieSym = ['\865','\860']
tieSpec = [Tiebar, Tiebar]

supSym = ['ː','ˑ','\774']
supSpec = [Long,
 HalfLong,
 ExtraShort]

tonSym = ['˥','˦','˧','˨','˩', '\779', '\769', '\772', '\768', '\783', '\780', '\770', 
  '\7620', '\7621', '\7624']
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

diaSym = ['\805', '\812', 'ʰ', '\825', '\796', '\799', '\800', '\776', '\829', '\809',
  '\815', '\734', '\804', '\816', '\828', 'ʷ', 'ʲ', 'ˠ', 'ˤ', '\820', '\797', '\798',
  '\792', '\793', '\810', '\826', '\827', '\771', 'ⁿ', 'ˡ', '\794', '\781', 'ʱ', 'ʴ', '\778',
  'ᵇ', 'ᵈ', 'ᶢ', 'ᶡ', 'ᵖ']
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
 AdvancedTongueroot,
 RetractedTongueroot,
 Dentalized,
 Apical,
 Laminal,
 Nasalized,
 NasalReleased,
 Lateral,
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

-- not in use, requires metric parser
preInventory = Map.fromList $ tuplify preSym preSpec
preInventory_inv = Map.fromListWith (++) $ (flip tuplify) (map (replicate 1) (preSym++preUniqueSym)) (preSpec++preUniqueSpec)

preSym = ['ᵇ', 'ᵈ', 'ᶢ', 'ᶡ', 'ᵐ', 'ⁿ', 'ᵑ', 'ᶮ', 'ᶯ']
preSpec = [BilabialVoicedPrestopped,
 AlveolarVoicedPrestopped,
 VelarVoicedPrestopped,
 PalatalVoicedPrestopped,
 BilabialPrenasalized,
 AlveolarPrenasalized,
 VelarPrenasalized,
 PalatalPrenasalized,
 RetroflexPrenasalized]

preUniqueSym = ['ꜜ', 'ꜝ', 'ꜛ', 'ꜞ', 'ꜟ']
preUniqueSpec = [Downstep,
 Downstep,
 Upstep,
 Upstep,
 Upstep]



--sounds
data Sound = C Place Manner Voicing | V Openness Frontness Roundedness | Sound [Sound]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Sound where
  showIPA (C p m v) = showIPA v++" "++showIPA p++" "++showIPA m
  showIPA (V o f r) = show o++" "++show f++" "++show r++" Vowel"
  showIPA (Sound xs) = "Sound "++(showL showIPA "[" ", " "]" "[empty list]" xs)

sound :: IPA -> Sound
sound (IPA s) = case length $ i of
  1 -> head i
  0 -> error "call to function 'sound' for empty"
  _ -> Sound i
  where 
    i = getSounds s
    getSounds [] = []
    getSounds (((s,_),_):xs) = [s] ++ getSounds xs
sound _ = error "call to function 'sound' for non-sound ipa"

isSound :: Sound -> IPA -> Bool
isSound snd ipa = (==) snd $ sound ipa

complexSound :: Sound -> [Sound]
complexSound (Sound ss) = ss

data Voicing = Voiced | Voiceless | Voicing [Voicing]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Voicing where
  showIPA (Voicing xs) = "Voicing "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA voicing = show voicing

-- for conosnants and vowels
uVoicing :: IPA -> Voicing
uVoicing ipa = assign Voicing voicing id complexSound sound ipa
  where 
    voicing (C _ _ voicing) = voicing
    voicing (V _ _ _) = Voiced

complexVoicing :: Voicing -> [Voicing]
complexVoicing (Voicing xs) = xs

data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Retroflex | Palatal | Velar | Uvular | Pharyngeal | Glottal | LabialVelar | LabialPalatal | Epiglottal | AlveoloPalatal | PalatoAlveolar | PostalveolarVelar | Place [Place]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Place where
  showIPA (Place xs) = "Place "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA place = tail $ concat $ intersperse "-" $ splitConserve isUpper $ show place

uPlace :: IPA -> Place
uPlace ipa = assign Place place id complexSound sound ipa
  where place (C place _ _) = place

complexPlace :: Place -> [Place]
complexPlace (Place xs) = xs

data Manner = Plosive | Nasal | Trill | Tap | Flap | Fricative | Approximant | Click | Implosive | Manner [Manner]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Manner where
  showIPA Click = ""
  showIPA Implosive = ""
  showIPA (Manner xs) = "Manner "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA manner = show manner

uManner :: IPA -> Manner
uManner ipa = assign Manner manner id complexSound sound ipa
  where manner (C _ manner _) = manner

complexManner :: Manner -> [Manner]
complexManner (Manner xs) = xs

data Openness = Close | CloseCloseMid | CloseMid | Mid | OpenMid | OpenOpenMid | Open | Openness [Openness]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Openness where
  showIPA (Openness xs) = "Openness "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA openness = tail $ concat $ intersperse "-" $ splitConserve isUpper $ show openness

uOpenness :: IPA -> Openness
uOpenness ipa = assign Openness open id complexSound sound ipa
  where open (V open _ _) = open

complexOpenness :: Openness -> [Openness]
complexOpenness (Openness xs) = xs

data Frontness = Front | FrontCentral | Central | BackCentral | Back | Frontness [Frontness]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Frontness where
  showIPA (Frontness xs) = "Frontness "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA frontness = tail $ concat $ intersperse "-" $ splitConserve isUpper $ show frontness

uFrontness :: IPA -> Frontness
uFrontness ipa = assign Frontness front id complexSound sound ipa
  where front (V _ front _) = front

complexFrontness :: Frontness -> [Frontness]
complexFrontness (Frontness xs) = xs

data Roundedness = Rounded | Unrounded | Roundedness [Roundedness]
  deriving (Eq, Show, Read, Ord)

instance ShowIPA Roundedness where
  showIPA (Roundedness xs) = "Roundedness "++(showL showIPA "[" ", " "]" "[empty list]" xs)
  showIPA roundedness = show roundedness

uRoundedness :: IPA -> Roundedness
uRoundedness ipa = assign Roundedness rounded id complexSound sound ipa
  where rounded (V _ _ rounded) = rounded

complexRoundedness :: Roundedness -> [Roundedness]
complexRoundedness (Roundedness xs) = xs

-- inventories for complex symbols
xInventory = Map.fromList xSource
xInventory_inv = Map.fromListWith (++) xSource_inv

xSource = tuplify xSym xSpec
xSource_inv = (flip tuplify) (map (replicate 1) xSym) xSpec
xSym = ['ɚ', 'ɝ','ǁ','ɬ','ɮ','l','ɭ','ʟ','ʎ','ɺ']
xSpec = [((V Mid Central Unrounded, PulmonicEgressive), [Rhotacized]), ((V OpenMid Central Unrounded, PulmonicEgressive),[Rhotacized]),  ((C Alveolar Click Voiceless, LingualIngressive), [Lateral]), ((C Alveolar Fricative Voiceless, PulmonicEgressive), [Lateral]), ((C Alveolar Fricative Voiced, PulmonicEgressive), [Lateral]), ((C Alveolar Approximant Voiced, PulmonicEgressive), [Lateral]), ((C Retroflex Approximant Voiced, PulmonicEgressive), [Lateral]), ((C Velar Approximant Voiced, PulmonicEgressive), [Lateral]), ((C Palatal Approximant Voiced, PulmonicEgressive), [Lateral]), ((C Alveolar Flap Voiced, PulmonicEgressive), [Lateral])]

-- inventories for simple symbols
sInventory = Map.fromList soundSource
sInventory_inv = Map.fromListWith (++) soundSource_inv
-- for looking up
sSym = peSym++giSym++liSym

soundSource = lingualingressive++glottalicingressive++pulmonicegressive
lingualingressive = tuplify liSym $ tuplify liSpec $ replicate (length liSpec) LingualIngressive
glottalicingressive = tuplify giSym $ tuplify giSpec $ replicate (length giSpec) GlottalicIngressive
pulmonicegressive = tuplify peSym $ tuplify peSpec $ replicate (length peSpec) PulmonicEgressive

soundSource_inv = lingualingressive_inv++glottalicingressive_inv++pulmonicegressive_inv
lingualingressive_inv = (flip tuplify) (map (replicate 1) liSym) $ tuplify liSpec $ replicate (length liSpec) LingualIngressive
glottalicingressive_inv = (flip tuplify) (map (replicate 1) giSym) $ tuplify giSpec $ replicate (length giSpec) GlottalicIngressive
pulmonicegressive_inv = (flip tuplify) (map (replicate 1) peSym) $ tuplify peSpec $ replicate (length peSpec) PulmonicEgressive

peSym = cSym++vSym
peSpec = cSpec++vSpec

giSym = ['ɓ','ɗ','ʄ','ɠ','ʛ']
giSpec = [C Bilabial Implosive Voiced,
 C Alveolar Implosive Voiced,
 C Palatal Implosive Voiced,
 C Velar Implosive Voiced,
 C Uvular Implosive Voiced]

liSym = ['ʘ','ǃ','ǂ','ǀ']
liSpec = [C Bilabial Click Voiceless,
 C Alveolar Click Voiceless,
 C PalatoAlveolar Click Voiceless,
 C Dental Click Voiceless]

vSym = ['i','y','ɨ','ʉ','ɯ','u','ɪ','ʏ','ʊ','e','ø','ɘ','ɵ','ɤ','o','ə',
  'ɛ','œ','ɜ','ɞ','ʌ','ɔ','æ','ɐ','a','ɶ','ɑ','ɒ']
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

cSym = ['p','b','m','ʙ','ɸ','β','ɱ','v','f','ⱱ','ʋ','θ','ð','t','d','n','r','ɾ',
  's','z','ɹ','ʃ','ʒ','ʈ','ɖ','ɳ','ɽ','ʂ','ʐ','ɻ','c','ɟ','ɲ','ç','ʝ',
  'j','k','g','ŋ','x','ɣ','ɰ','q','ɢ','ɴ','ʀ','χ','ʁ','ħ','ʕ','ʔ','h','ɦ',
  'ʍ','w','ɥ','ʜ','ʢ','ɕ','ʑ','ʡ','ɡ','ɧ']
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
 C Alveolar Approximant Voiced,
 C Postalveolar Fricative Voiceless,
 C Postalveolar Fricative Voiced,
 C Retroflex Plosive Voiceless,
 C Retroflex Plosive Voiced,
 C Retroflex Nasal Voiceless,
 C Retroflex Tap Voiceless,
 C Retroflex Fricative Voiceless,
 C Retroflex Fricative Voiced,
 C Retroflex Approximant Voiced,
 C Palatal Plosive Voiceless,
 C Palatal Plosive Voiced,
 C Palatal Nasal Voiced,
 C Palatal Fricative Voiceless,
 C Palatal Fricative Voiced,
 C Palatal Approximant Voiced,
 C Velar Plosive Voiceless,
 C Velar Plosive Voiced,
 C Velar Nasal Voiced,
 C Velar Fricative Voiceless,
 C Velar Fricative Voiced,
 C Velar Approximant Voiced,
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
 C Epiglottal Plosive Voiceless,
 C Velar Plosive Voiced,
 C PostalveolarVelar Fricative Voiceless]
