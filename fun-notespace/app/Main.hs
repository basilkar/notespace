module Main where

import System.IO

-- third party libs
import Data.Time (getCurrentTime)

-- local libs
import Guitar
import InductiveChords
import Lib
import Notes
import Rhythm
import Tymoczko

main :: IO ()
main = do
    putStrLn "Notes' Pace (yet another music app)"
    todo

todo :: IO ()
todo = do
    time <- getCurrentTime
    putStrLn (show time)
    putStrLn "TODO:"
    putStrLn "1. organize the haskell part of the project"
    putStrLn "2. finish implementing Tymoczko's stuff"

printConfig :: IO ()
printConfig = do
    contents <- readFile "stack.yaml"
    putStrLn contents
