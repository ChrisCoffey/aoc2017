module Solutions.Quadcopter (
    partA,
    partB
) where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (isJust, maybe)
import Text.Parsec
import Text.Parsec.String (Parser)

newtype Bots = Bots (M.Map Int Drone)
    deriving (Show, Eq)
newtype Outputs = Outputs (M.Map Int (Maybe Int))
    deriving (Show, Eq)

data Drone 
    = Drone {
        low :: Maybe Int, 
        high :: Maybe Int,
        lowRule :: Bots -> Outputs -> (Bots, Outputs),
        highRule :: Bots  -> Outputs -> (Bots, Outputs)} 
   
instance Show Drone where
    show (Drone l h _ _ ) = "Drone (" ++ show l ++ " " ++ show h ++ ")" 

instance Eq Drone where
    (Drone l h _ _) == (Drone l' h' _ _ ) = l == l' && h == h'

quadcopterAttack :: IO (Bots, Outputs)
quadcopterAttack = do
    ls <- lines <$> readFile "data/Quadcopter.rules"
    let rules = unlines $ filter (elem "gives" . words) ls
    let inits = unlines $ filter (elem "value" . words) ls
    case runParser droneParse () "" rules of
        Left e -> error $ show e
        Right drones -> 
            case runParser parseInits () "" inits of
                Left e -> error $ show e
                Right inits -> do
                    let outputs = Outputs $ M.fromList [(x,Nothing)| x <- [0..30]]
                    let bots = Bots $ M.fromList drones
                    let bots' = applyInits inits bots
                    let res = runRules bots' outputs
                    pure res
    where
    applyInits [] bots = bots
    applyInits (f:fs) bots = applyInits fs (f bots)

partA :: IO ()
partA = do
    (bots, outputs) <- quadcopterAttack
    print bots
          
partB :: IO ()
partB = do
    (bots, outputs) <- quadcopterAttack
    print outputs

runRules :: Bots -> Outputs -> (Bots, Outputs)
runRules = go S.empty
    where
    go seen b@(Bots bs) o@(Outputs os) = let
        (b', o') = foldl f (b,o) toProcess
        seen' = seen `S.union` S.fromList (M.keys toProcess)
        in if b' == b && o' == o
           then (b', o') 
           else go seen' b' o'
        where
        f (bots, outputs) d = let
            (b',o') = (lowRule d) bots outputs
            in (highRule d) b' o'
        unseen = M.filterWithKey (\k d -> not $ S.member k seen) bs
        fulfilled d = (isJust $ low d) && (isJust $ high d)
        toProcess = M.filter fulfilled unseen

parseInits :: Parser [(Bots -> Bots)]
parseInits = sepEndBy parseDroneInit newline

droneParse :: Parser [(Int, Drone)]
droneParse = sepEndBy parseDroneRules newline

parseDroneInit :: Parser (Bots -> Bots)
parseDroneInit = do
    _ <- string "value"
    space
    v <- many1 digit
    let value = read v
    space
    _ <- string "goes to bot"
    space
    d <- many1 digit
    let droneId = read d
    pure $ f droneId value
    where
    f dId v (Bots bs) = let
        drone = bs M.! dId
        drone' = giveToDrone drone (Just v)
        in Bots $ M.insert dId  drone' bs

parseDroneRules :: Parser (Int, Drone)
parseDroneRules = do
    _ <- string "bot"
    space
    d <- many1 digit
    let droneId = read d 
    space
    _ <- string "gives low to"
    space
    lowRule <- parseDroneRule True droneId
    space
    _ <- string "and high to"
    space
    highRule <- parseDroneRule False droneId
    pure (droneId, Drone Nothing Nothing lowRule highRule)


parseDroneRule :: Bool -> Int -> Parser (Bots -> Outputs -> (Bots, Outputs))
parseDroneRule isLow dId = do
    target <- many1 letter
    space
    d <- many1 digit
    let targetId = read d :: Int
    case target of
        "bot" -> pure $ f False  targetId
        _ -> pure $ f True targetId
    where
    f True tId bs@(Bots bots) ot@(Outputs out) = let
        drone = bots M.! dId
        v = if isLow then low drone else high drone
        in (bs, Outputs $ M.insert tId v out)
    f False tId bs@(Bots bots) ot@(Outputs out) = let
        drone = bots M.! dId
        v = if isLow then low drone else high drone
        t = bots M.! tId
        t' = giveToDrone t v
        in (Bots $ M.insert tId t' bots, ot)

giveToDrone :: Drone -> Maybe Int -> Drone
giveToDrone d Nothing  = error "Must have a value to give one away"
giveToDrone d (Just n) 
    | maybe False (< n) (low d) = d {high = Just n}
    | maybe False (> n) (low d) = d {low = Just n, high = low d}
    | maybe False (< n) (high d) = d {low = high d, high = Just n}
    | maybe False (> n) (high d) = d {low = Just n}
    | otherwise = d {low = Just n}

