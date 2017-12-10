{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions17.Day7 where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Data.Map as M

data RoseTree =
    Node {name :: T.Text, weight:: Int, children:: [T.Text]}
    | Leaf {lName :: T.Text, lWeight :: Int}
    deriving (Show, Eq, Ord)

loadTree :: IO [RoseTree]
loadTree = do
    raw <- readFile "data/2017/Day7.a"
    case parse parseTreeElements "Tree data" raw of
        Left e -> error $ show e
        Right tree -> pure tree

buildRootMap ::
    [RoseTree]
    -> M.Map T.Text T.Text
buildRootMap nodes =
    foldr addPath M.empty $ filter isNode nodes
    where
    isNode (Node {..}) = True
    isNode _ = False
    addPath (Node {..}) m = foldr (\ k m -> M.insert k name m) m children

buildNodeIndex ::
    [RoseTree]
    -> M.Map T.Text RoseTree
buildNodeIndex = foldr (\ n m -> M.insert (nodeName n) n m) M.empty
    where
        nodeName Node {..} = name
        nodeName Leaf {..} = lName

buildTree ::
    T.Text -- | Root node
    -> M.Map T.Text RoseTree


findRoot ::
    T.Text
    -> M.Map T.Text T.Text
    -> T.Text
findRoot key m =
    case  key `M.lookup` m of
        Nothing -> key
        Just key' -> findRoot key' m

-- zuruj is an arbitrary node. Any node in the tree will work
-- result is bsfpjtc
checkA :: IO T.Text
checkA = findRoot "zuruj" . buildRootMap <$> loadTree

-- | parser
--

parseTreeElements :: Parser [RoseTree]
parseTreeElements =
    (try parseNode <|> parseLeaf) `sepEndBy` endOfLine

parseNode :: Parser RoseTree
parseNode = do
    name <- parseName
    space
    weight <- parseWeight
    string " -> "
    children <- parseChildren
    pure Node {..}

parseLeaf :: Parser RoseTree
parseLeaf = do
    name <- parseName
    space
    weight <- parseWeight
    pure $ Leaf name weight

parseName :: Parser T.Text
parseName =
    T.pack <$> many letter

parseWeight :: Parser Int
parseWeight = do
    char '('
    n <- read <$> many1 digit
    char ')'
    pure n

parseChildren :: Parser [T.Text]
parseChildren =
    parseName `sepBy` (string ", ")
