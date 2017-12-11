module Solutions17.Day8 where

import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

type Registers = M.Map Register Int

newtype Register = Reg T.Text
    deriving (Show, Eq, Ord)

data Cond
    = Gt Register Int
    | Lt Register Int
    | Eq Register Int
    | Neq Register Int
    | Or Cond Cond
    deriving (Show, Eq, Ord)

data Instruction
    = Inc Register Int Cond
    | Dec Register Int Cond
    deriving (Show, Eq, Ord)

evalInstruction ::
    Registers
    -> Instruction
    -> Registers
evalInstruction rx (Inc r n cond)
    | evalCond cond rx = M.insertWith (+) r n rx
    | otherwise = rx
evalInstruction rx (Dec r n cond)
    | evalCond cond rx && M.member r rx = M.insertWith (flip (-)) r n rx
    | evalCond cond rx = M.insert r (0 - n) rx
    | otherwise = rx

evalCond ::
    Cond
    -> Registers
    -> Bool
evalCond (Gt r n) rx =
    (> n) . fromMaybe 0 $ M.lookup r rx
evalCond (Lt r n) rx =
    (< n) . fromMaybe 0 $ M.lookup r rx
evalCond (Eq r n) rx =
    (== n) . fromMaybe 0 $ M.lookup r rx
evalCond (Neq r n) rx =
    (/= n) . fromMaybe 0 $ M.lookup r rx
evalCond (Or l r) rx =
    evalCond l rx || evalCond r rx
--
-- Parser
--

pInstructions :: Parser [Instruction]
pInstructions =
    sepEndBy pInstruction endOfLine

pInstruction :: Parser Instruction
pInstruction = do
    reg <- pRegister
    space
    op <- pOP
    space
    n <- pNum
    space
    cond <- pCond
    pure $ op reg n cond

    where
        pOP = do
            cx <- string "inc" <|> string "dec"
            case cx of
                "inc" -> pure Inc
                "dec" -> pure Dec
                _ -> unexpected "Not a valid instruction"

pCond :: Parser Cond
pCond = do
    string "if"
    space
    reg <- pRegister
    space
    op <- pOP
    space
    n <- pNum
    pure $ op reg n

    where
        pOP = do
            cx <- many1 (char '>' <|> char '<' <|> char '=' <|> char '!')
            case cx of
                ">" -> pure Gt
                "<" -> pure Lt
                ">=" -> pure $ \r n -> Or (Gt r n) (Eq r n)
                "<=" -> pure $ \ r n -> Or (Lt r n) (Eq r n)
                "!=" -> pure Neq
                "==" -> pure Eq
                _ -> unexpected "Not a valid operator"

pRegister :: Parser Register
pRegister = do
    name <- many alphaNum
    pure . Reg $ T.pack name

pNum :: Parser Int
pNum =
    read <$> many1 (char '-' <|> digit)

testCase :: String
testCase =
    "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\nc inc 15 if c != 0\nc dec 2 if c <= 10"

--
-- Programs
--
checkA :: IO ()
checkA = do
    raw <- readFile "data/2017/Day8.a"
    case parse pInstructions "Program" raw of
        Left e -> print e
        Right ixs -> do
            print $ length ixs
            let m = foldl' evalInstruction M.empty ixs
                res = maximum $ snd <$> M.toList m
            print res
