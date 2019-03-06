module IPA_util

where

import Data.List

-- operating on lists
satisfy :: Foldable t => (a -> Bool) -> t a -> Bool
satisfy f ipal = not $ foldl (flip ((==) . f)) False ipal

elemIndexSatisfy :: (a -> Bool) -> [a] -> [Int]
elemIndexSatisfy f ipal = jumperate 0 $ map f ipal 
  where jumperate _ [] = []
        jumperate n b = case elemIndex False b of
          Just i  -> [n+i] ++ jumperate (i+n+1) (drop (i+1) b)
          Nothing -> []

-- creates tuples out of lists, safe version of zip
tuplify :: [a] -> [b] -> [(a, b)]
tuplify [] [] = []
tuplify (x:xs) [] = error "mismatched tuples"
tuplify [] (y:ys) = error "mismatched tuples"
tuplify (x:xs) (y:ys) = [(x,y)] ++ tuplify xs ys

-- checks if all elements of a list are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- converts a list into an explicit output format, used for strings
showL :: (t -> [a]) -> [a] -> [a] -> [a] -> [a] -> [t] -> [a]
showL f init inter term alt [] = alt
showL f init inter term alt (x:xs) = init ++(f x)++ showL2 xs
  where showL2 [] = term
        showL2 (x:xs) = inter++(f x)++showL2 xs

-- selects a default value
defaultize :: p -> [p] -> p
defaultize base [] = base
defaultize base x = catch x
  where 
    catch x = case length x of
      1 -> head x
      _ -> error "conflicting and/or redundant modifiers"

-- abstract for general partial matches (a,b) -> [(a,b)] -> ((a,b),b), set of functions? partial match boolean, dependency on splitWith limits flexibility
--partial :: ((Sound,Airstream), [Modif]) -> [((Sound,Airstream), [Modif])] -> (((Sound, Airstream), [Modif]), [Modif])
partial :: (Eq a1, Eq a2, Foldable t) => (a1, [a2]) -> [(a1, t a2)] -> ((a1, [a2]), [a2])
partial (s,m) [] = ((s,[]),m)
partial (s,m) ((s2,m2):sms) | ((s == s2) && (matched /= [])) = ((s,matched),unmatched)
                            | otherwise = partial (s,m) sms
  where
    (matched, unmatched) = splitWith ((flip elem) m2) m

splitWith :: (a -> Bool) -> [a] -> ([a],[a]) 
splitWith f l = splitWith' f l [] []

splitWith' :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a]) 
splitWith' _ [] alpha beta = (alpha, beta)
splitWith' f (x:xs) alpha beta = if f x then splitWith' f xs (alpha++[x]) beta else splitWith' f xs alpha (beta++[x])

splitConserve :: (a -> Bool) -> [a] -> [[a]]
splitConserve f xs = splitConserve' f [] [] xs

splitConserve' :: (a -> Bool) -> [a] -> [[a]] -> [a] -> [[a]]
splitConserve' f temp out [] = out ++ [temp]
splitConserve' f temp out (x:xs) = if f x then splitConserve' f [x] (out++[temp]) xs else splitConserve' f (temp++[x]) out xs








