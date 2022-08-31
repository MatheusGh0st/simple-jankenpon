module Main (main) where

data RPS = Rock | Paper | Scissors
        deriving (Read, Show, Eq)

data Game = Player1 | Player2 | Draw
        deriving (Read, Show)

instance Ord RPS where
    Rock     `compare` Scissors = GT
    Scissors `compare` Rock     = LT

    Paper    `compare` Rock     = GT
    Rock     `compare` Paper    = LT

    Scissors `compare` Paper    = GT
    Paper    `compare` Scissors = LT

    _        `compare` _        = EQ

rps :: RPS -> RPS -> Game
rps p1 p2 =
    case p1 `compare` p2 of
        EQ -> Draw
        GT -> Player1
        LT -> Player2

main :: IO ()
main = do
    in1 <- getLine
    in2 <- getLine
    let p1 = read in1 :: RPS
    let p2 = read in2 :: RPS
    putStrLn $ show (rps p1 p2)
    