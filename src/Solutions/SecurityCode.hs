module Solutions.SecurityCode (
    securityCodeA,
    securityCodeB,
) where

data Instruction = R |  L | U | D
    deriving (Show)

keyPadMove :: Int -> Instruction -> Int
keyPadMove 1 R = 2
keyPadMove 1 D = 4
keyPadMove 2 L = 1
keyPadMove 2 R = 3
keyPadMove 2 D = 5
keyPadMove 3 L = 2
keyPadMove 3 D = 6
keyPadMove 4 R = 5
keyPadMove 4 U = 1
keyPadMove 4 D = 7
keyPadMove 5 U = 2
keyPadMove 5 L = 4
keyPadMove 5 R = 6
keyPadMove 5 D = 8
keyPadMove 6 U = 3
keyPadMove 6 L = 5
keyPadMove 6 D = 9
keyPadMove 7 U = 4
keyPadMove 7 R = 8
keyPadMove 8 L = 7
keyPadMove 8 U = 5
keyPadMove 8 R = 9
keyPadMove 9 U = 6
keyPadMove 9 L = 8
keyPadMove n _ = n

keyPadMove' :: Char -> Instruction -> Char
keyPadMove' '1' D = '3'
keyPadMove' '2' R = '3'
keyPadMove' '2' D = '6'
keyPadMove' '3' U = '1'
keyPadMove' '3' L = '2'
keyPadMove' '3' R = '4'
keyPadMove' '3' D = '7'
keyPadMove' '4' L = '3'
keyPadMove' '4' D = '8'
keyPadMove' '5' R = '6'
keyPadMove' '6' L = '5'
keyPadMove' '6' R = '7'
keyPadMove' '6' U = '2'
keyPadMove' '6' D = 'A'
keyPadMove' '7' L = '6'
keyPadMove' '7' R = '8'
keyPadMove' '7' D = 'B'
keyPadMove' '7' U = '3'
keyPadMove' '8' R = '9'
keyPadMove' '8' L = '7'
keyPadMove' '8' U = '4'
keyPadMove' '8' D = 'C'
keyPadMove' '9' L = '8'
keyPadMove' 'A' U = '6'
keyPadMove' 'A' R = 'B'
keyPadMove' 'B' U = '7'
keyPadMove' 'B' L = 'A'
keyPadMove' 'B' R = 'C'
keyPadMove' 'B' D = 'D'
keyPadMove' 'C' U = '8'
keyPadMove' 'C' L = 'B'
keyPadMove' 'D' U = 'B'
keyPadMove' c _ = c

computeCode :: [[Instruction]] -> [Int]
computeCode = reverse . snd . foldl f (5, []) 
    where
    f (k, acc) is = (\x -> (x,x:acc )) $ runRow is k
    runRow is key = foldl keyPadMove key is

computeCode' :: [[Instruction]] -> [Char]
computeCode' = reverse . snd . foldl f ('5', []) 
    where
    f (k, acc) is = (\x -> (x,x:acc )) $ runRow is k
    runRow is key = foldl keyPadMove' key is

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions ('R':xs) = R:parseInstructions xs
parseInstructions ('U':xs) = U:parseInstructions xs 
parseInstructions ('D':xs) = D:parseInstructions xs
parseInstructions ('L':xs) = L:parseInstructions xs

securityCodeA :: IO ()
securityCodeA = do
    ls <- lines <$> readFile "data/SecurityCode.txt"
    let instructions = parseInstructions <$> ls
    let code = computeCode instructions
    print $ show code


securityCodeB :: IO ()
securityCodeB = do
    ls <- lines <$> readFile "data/SecurityCode.txt"
    let instructions = parseInstructions <$> ls
    let code = computeCode' instructions
    print $ show code
