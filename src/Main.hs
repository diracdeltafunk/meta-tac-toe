module Main where

import Control.Applicative
import Game

main :: IO ()
main = do
    let board = newGame
    putStrLn "New Game:"
    print board
    putStrLn "Supersector in which to place first move:"
    sector <- read <$> getLine
    putStrLn "Subsector in which to place first move:"
    place <- read <$> getLine
    let newBoard = go (move X (sector, place)) board
    putStrLn "New Board State:"
    print $ snd newBoard
    makeMove O newBoard
    where makeMove mark c@(sector, board) = do putStrLn $ "Choose a subsector to move in within main sector " ++ (show sector) ++ ":"
                                               place <- read <$> getLine
                                               if (uncurry legal) c place
                                                   then do let newBoard@(_,b) = go (move mark (sector, place)) board
                                                           putStrLn "New Board State:"
                                                           print b
                                                           case claimed b of
                                                               []     -> if full b then putStrLn "NOBODY WINS!" else makeMove (if mark == X then O else X) newBoard
                                                               (m:[]) -> putStrLn $ (show m) ++ " WINS!"
                                                               _      -> putStrLn "EVERYONE WINS!"
                                                   else do putStrLn "Illegal Move!"
                                                           makeMove mark c
