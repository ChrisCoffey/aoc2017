module Solutions.TLS (
    abbA,
    abbB
) where

import qualified Data.Set as S

import Debug.Trace

abbA :: IO ()
abbA = do
    ls <- lines <$> readFile "data/TLS.txt"
    let n = length $ filter (\l -> isAbba l False False) ls
    print $ show n

abbB :: IO ()
abbB = do
    ls <- lines <$> readFile "data/TLS.txt"
    let n = length $ filter isAba ls
    print $ show n

isAbba :: String -> Bool -> Bool -> Bool
isAbba (a:b:c:[]) found _ = found
isAbba (a:b:c:d:rest) found inBraces
    | a == '[' = isAbba (b:c:d:rest) found True
    | a == ']' = isAbba (b:c:d:rest) found False
    | matches && inBraces = False
    | otherwise = isAbba (b:c:d:rest) (found || matches && not inBraces) inBraces
    where
    matches = a == d && b == c && a /= b
   
isAba :: String -> Bool
isAba s = let 
    a = supers s
    b = hypers (dropWhile (/= '[') s)
    in not . null $ (trace (show a) a) `S.intersection` (trace (show b) b)

supers :: String -> S.Set String
supers [a,b] = S.empty
supers (a:b:'[':rest) = supers (dropWhile (/= ']') rest)
supers (']':rest) = supers rest
supers (a:b:c:rest) 
    | a == c && a /= b = S.insert [b,a,b] (supers (b:c:rest))
    | otherwise = supers (b:c:rest)

hypers :: String -> S.Set String
hypers [] = S.empty
hypers [a,b] = S.empty
hypers (a:b:']':rest) = hypers (dropWhile (/= '[') rest)
hypers ('[':rest) = hypers rest
hypers (a:b:c:rest)
    | a == c && a /= b = S.insert [a,b,c] (hypers (b:c:rest))
    | otherwise = hypers (b:c:rest)

tripples :: String -> S.Set String
tripples s = go s S.empty
    where
    go [a,b] acc = acc
    go (a:b:c:rest) acc = go (b:c:rest) $ S.insert [a,b,c] acc
