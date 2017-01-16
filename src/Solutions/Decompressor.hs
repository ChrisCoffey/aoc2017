module Solutions.Decompressor (
    partA,
    partB
) where

import Debug.Trace

partA :: IO ()
partA = do
    xs <- readFile "data/Decompressor.txt"
    let res = process xs
    print res
    print . show $ length res

partB :: IO ()
partB = do
    xs <- readFile "data/Decompressor.txt"
    let res = processRec xs
    print $ show res

process :: String -> String
process = go []
    where
    go out "" = out
    go out ('(':rest) = let
        (xs, (len, reps)) = readMarker rest
        (xs', block) = captureBlock xs len
        in flip go xs' $ out ++ (concat $ replicate reps block) 
    go out (c:rest) = go (out ++ [c]) rest


processLength :: String -> Int
processLength = go 0
    where
    go n "" = n
    go n ('(':rest) = let
        (xs, (len, reps)) = readMarker rest
        (xs', block) = captureBlock xs len
        blockLength = reps *  processLength block
        in flip go xs' $ n + blockLength
    go n (_:rest) = go (n + 1) rest

processRec :: String -> Int
processRec = sum . fmap processBlock . splitBlocks

processBlock :: (Int, String) -> Int
processBlock (n, block) = 
    if length additionalBlocks == 1
    then n * (fst $ head additionalBlocks)
    else n * sum (processBlock <$> additionalBlocks)
    where
    additionalBlocks = splitBlocks block

readMarker :: String -> (String, (Int, Int))
readMarker s = let 
        block = takeWhile (/= ')') s
        len = read $ takeWhile (/= 'x') block
        times = read . tail $ dropWhile (/= 'x') block
        in (rest, (len, times))
    where
    rest = tail $ dropWhile (/= ')') s

captureBlock :: String -> Int -> (String, String)
captureBlock = go "" 
    where
    go captured rest 0 = (rest, reverse captured)
    go captured [] n = ("", reverse captured)
    go captured (c:rest) n = go (c:captured) rest (n -1)

splitBlocks :: String -> [(Int, String)]
splitBlocks s = let 
    (n, blocks) = go [] 0 s
    in (n, "a"):blocks
    where
    go blocks n "" = (n, blocks)
    go blocks n ('(':rest) = let
        (xs, (len, reps)) = readMarker rest
        (xs', block) = captureBlock xs len
        in go ((reps, block):blocks) n xs'
    go blocks n (_:rest) = go blocks (n+1) rest
