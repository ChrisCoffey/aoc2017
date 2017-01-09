module Solutions.Chess (
    hashLoopA,
    hashLoopB,
    hashLoopB'
) where

import Data.Hash.MD5 
import Data.Ord (comparing)
import qualified Data.Array as A
import Numeric (readHex)

import Debug.Trace

hashLoopA = fmap (!! 5) . take 8 . filter isInteresting $ hashes

hashLoopB = take 32 . filter isInteresting $ hashes

hashes :: [String]
hashes =  md5s . Str . ("wtnhxymk" ++) . show <$> [1..]

isInteresting :: String -> Bool
isInteresting ('0':'0':'0':'0':'0':rest) = True
isInteresting _ = False

hashLoopB' :: A.Array Int Char
hashLoopB' = go 1 (A.array (0, 7) ([0..7] `zip` repeat 'z'))
    where
    h = md5s . Str . ("wtnhxymk" ++) . show
    trySet arr ix c 
        | arr A.! ix == 'z' = (trace (show arr) arr) A.// [(ix, c)]
        | otherwise = arr
    go :: Int -> A.Array Int Char -> A.Array Int Char
    go n arr 
        | notElem 'z' arr =  arr
        | otherwise = let
            hash = h n
            i = fst . head $ readHex [hash !! 5]
            c = hash !! 6
            in if startsWith "00000" hash && i <= 7
            then go (n + 1) (trySet arr i c)
            else go (n + 1) arr
        

startsWith :: String -> String -> Bool
startsWith [] s = True
startsWith p [] = True
startsWith (p:rest) (s:xs)
    | p == s = startsWith rest xs
    | otherwise = False
