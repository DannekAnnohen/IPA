module IPA

  (

-- parsers
  parseIPA, parseSym, parse1, 

-- backwards parsers
  backparseIPA,

-- auxiliary functions
  present,

  module IPA_data,
  module IPA_util,
  )

where

-- I use this package to implement IPA symbol-meaning mapping dictionaries. A qualified import means that whenever I call a function from this package I have to write it like so : Map.function
import qualified Data.Map.Lazy as Map
import Data.List
import Data.Maybe
import IPA_data
import IPA_util (tuplify, showL, partial, satisfy, elemIndexSatisfy)

-- parser for [Char] to [IPA], view output with function present
-- firstlevel check with xSym seems silly, is it really faster?
parseIPA :: [Char] -> [IPA]
parseIPA str = spellback str [] [] []

spellback :: [Char] -> [(Sound,Airstream)] -> [Modif] -> [IPA] -> [IPA]
spellback [] zw dia out = (out ++ mergeIPA zw dia)
spellback (x:xs) zw dia out | x `elem` sSym = if checklast Tiebar dia 
  then spellback xs (zw ++ [sTemp]) (dia ++ [TieEnd]) out 
  else spellback xs [sTemp] [] (out ++ mergeIPA zw dia)

                            | x `elem` xSym = if checklast Tiebar dia
  then spellback xs (zw ++ [fst xTemp]) (dia ++ (snd xTemp)) out --doesn't need TieEnd
  else spellback xs [fst xTemp] (snd xTemp) (mergeIPA zw dia ++ out)

                            | x `elem` mSym = spellback xs zw (dia ++ [mTemp]) out

                            | x `elem` bSym = spellback xs [] [] (out ++ mergeIPA zw dia ++ [bTemp])

                            | x == ejm = spellback xs (ejectify zw) dia out

                            | otherwise = spellback xs [] [] (out ++ mergeIPA zw dia ++ [Unreadable x])
  where 
    sTemp = fromJust $ Map.lookup x sInventory
    mTemp = fromJust $ Map.lookup x mInventory
    bTemp = fromJust $ Map.lookup x bInventory
    xTemp = fromJust $ Map.lookup x xInventory
    ejectify = (uncurry (++)) . (\ (xs,[(s,_)]) -> (xs,[(s,GlottalicEgressive)])) . splitAt (length zw-1) 
    checklast i l = if (&&) ((&&) (i `elem` l) $ (==) i $ last l) $ zw /= [] then True else False

mergeIPA :: [(Sound, Airstream)] -> [Modif] -> [IPA]
mergeIPA [] dia = map IPAm $ dia
mergeIPA sa dia = flip (:) [] $ IPA $ tuplify sa $ cut $ filter (/= TieEnd) dia
  where 
    cut [] = [[]]
    cut x = case elemIndex Tiebar x of
      Just int -> [take int x] ++ cut (drop (int+1) x)
      Nothing  -> [x]


-- checking for complex defined characters proved notoriously difficult. has to check every complex entry before resorting to sInventory. solution: implement partial lookup function for Data.Map and unify xInventory and sInventory
backparseIPA :: [IPA] -> [Char]
backparseIPA [] = ""
backparseIPA (b@(IPAb _):ipas) = (fromJust $ Map.lookup b bInventory_inv) : backparseIPA ipas
backparseIPA ((Unreadable c):ipas) = [c] ++ backparseIPA ipas
backparseIPA ((IPAm m):ipas) = fromJust $ Map.lookup m mInventory_inv
backparseIPA (ipa:ipas) = (assign (showL id "" "\865" "" "") --(assign constr
  (\x -> lookupSound (fst x) ++ lookupModifiers (snd x)) --assignF
  (flip partial xSpec) --filterF
  (\((s,a),m) -> ((flip tuplify) (complexModifiers m) $ tuplify (complexSound s) $ complexAirstream a)) --merge
  (\x -> ((sound x, airstream x), modifiers x)) ipa) ++ backparseIPA ipas --extract) ++ recursion
  where
    lookupModifiers m = concat $ map (take 1) $ map fromJust $ map ((flip Map.lookup) mInventory_inv) m
    lookupSound i@((s,a),m) = if a == GlottalicEgressive then lookupSound' ((s,PulmonicEgressive),m) ++ [ejm] else lookupSound' i
    lookupSound'  ((s,a),m) | m == [] = case Map.lookup (s,a) sInventory_inv of
      Just sound -> take 1 sound
      Nothing    -> "#Unknown#"
                            | otherwise = case Map.lookup ((s,a),m) xInventory_inv of
      Just sound -> take 1 sound
      Nothing    -> error "fatal mismatch in xInventory_inv xSpec"


-- parses a single sound
parse1 :: [Char] -> IPA
parse1 str = case length temp of
  0 -> error "input does not constitute a sound"
  _ -> head temp
  where temp = parseIPA str


-- returns the specifications for a single symbol
parseSym :: Char -> IPA
parseSym x      | x `elem` sSym = case Map.lookup x sInventory of
      Just temp -> IPAs (temp, [])
      Nothing   -> Unreadable x

                | x `elem` mSym = case Map.lookup x mInventory of
      Just temp -> IPAm temp
      Nothing   -> Unreadable x

                | x `elem` bSym = case Map.lookup x bInventory of
      Just temp -> temp
      Nothing   -> Unreadable x

                | x `elem` xSym = case Map.lookup x xInventory of
      Just temp -> IPAs temp
      Nothing   -> Unreadable x

                | x == ejm = IPAm EjectivityMarker
                | otherwise = Unreadable x


-- formatting [IPA]
present [] = putStr ""
present (x:xs) = case typing x of
  Boundary -> case boundary x of
    Whitespace -> do 
      putStrLn $ "\n\n"++(showIPA x)++"\n\n"
      present xs
    Undertie -> simpleout
    MajorGroup -> do 
      putStrLn $ "\n\n"++(showIPA x)++"\n\n"
      present xs
    MinorGroup -> do
      putStrLn $ "\n"++(showIPA x)++"\n"
      present xs
    PrimaryStress -> simpleout
    SecondaryStress -> simpleout
    SyllableBoundary -> simpleout
    GlobalRise -> simpleout
    GlobalFall -> simpleout
  _ -> simpleout
  where simpleout = do {putStrLn $ showIPA x ; present xs}
