module DeBruijn where

import Control.Exception
import System.Exit
import System.IO
import Data.Bool
import Data.List

-- Will calculate next word when lenght != n
-- 'take' takes n letters from 'last' string
calcNextWord :: Int -> String -> String
calcNextWord n lastS = if length lastS >= n
                    then take n lastS
                    else calcNextWord n $ lastS ++ lastS

-- Function that deletes the last character if last in alphabet and add + 1 in alphabet
upOrDelete initS lastS = if lastS /= [] && last (lastS) == last (initS)
                         then do upOrDelete initS $ init lastS
                         else if lastS == [] then [] else init (lastS) ++ [initS!!((getElemIndexFromInit (last (lastS)) initS 0) + 1)]

--getElemIndexFromInit :: String -> Char -> Int
getElemIndexFromInit c str i
                     | length (str) == i = 0
                     | c == str!!i = i
                     | otherwise = getElemIndexFromInit c str $ i + 1

-- starting point for the generation algorithm
generateAlgorithm :: Int -> String -> String -> String
generateAlgorithm n initS lastS
                  | lastS == [] = lastS
                  | n `mod` length (lastS) == 0 = lastS ++ (generateAlgorithm n initS $ upOrDelete initS $ calcNextWord n lastS)
                  | otherwise = generateAlgorithm n initS $ upOrDelete initS $ calcNextWord n lastS

-- Checks if all tags are in the generated sequence
checkIfInSequence :: (Eq a) => [a] -> [a] -> Bool
checkIfInSequence x y = if x == [] || y == []
                        then False
                        else null (x \\ y) && null (y \\ x)

-- Compare two sequences to determine if str2 is a real sequence or not
getAllCombinations :: Int -> String -> String -> [String] -> Int -> [String]
getAllCombinations b str1 str2 str3 n
                 | n >= length str1 = str3
                 | length str1 /= length str2 = []
                 | otherwise = str3 ++ (getAllCombinations b str1 ([last str2] ++ init str2) [take b (cycle ([last str2] ++ init str2))] $ n + 1)

-- Execute verification of sequence
isASequence :: Int -> String -> String -> Bool
isASequence b alp seq = checkIfInSequence (getAllCombinations b seq (generateAlgorithm b alp [(alp!!0)]) [] 0) (getAllCombinations b (generateAlgorithm b alp [(alp!!0)]) seq [] 0)

-- check with circular arrengement
checkCircular :: Int -> String -> String -> Bool
checkCircular n x y
              | n >= length x = False
              | x == y = True
              | otherwise = checkCircular (n + 1) x ([last y] ++ init y)

-- Check if input reached EOF
getCurrentInput :: IO String
getCurrentInput = do a <- isEOF
                     if a
                     then displayHelp
                     else getLine

-- Checks if the sequence entered is a real deBruijn sequence or not
checkAlgorithm :: Int -> String -> IO ()
checkAlgorithm b alp = if checkAlphaUnique alp 0 == False
                       then displayHelp
                       else do seq <- getCurrentInput
                               if isASequence b alp seq == True
                               then putStrLn "OK"
                               else putStrLn "KO"

-- display lyndon words of a deBruijn sequence
lyndonAlgorithm :: Int -> String -> IO ()
lyndonAlgorithm b alp = if checkAlphaUnique alp 0 == False
                       then displayHelp
                       else do seq <- getCurrentInput
                               if isASequence b alp seq == True
                               then do putStrLn $ "All combinations of Lyndon words in " ++ seq ++ " :"
                                       print $ getAllCombinations b seq seq [] 0
                               else putStrLn $ "\x1b[31m" ++ seq ++ " isn't a deBruijn sequence." ++ "\x1b[0m"

-- check with circular arrengement
computeCircular :: Int -> String -> IO ()
computeCircular n y
              | n >= length y = putStrLn ""
              | otherwise = do putStrLn y ; computeCircular (n + 1) ([last y] ++ init y)

-- display lyndon words of a deBruijn sequence
circularAlgorithm :: Int -> String -> IO ()
circularAlgorithm b alp = if checkAlphaUnique alp 0 == False
                       then displayHelp
                       else do seq <- getCurrentInput
                               if isASequence b alp seq == True
                               then do putStrLn $ "All circular variations of " ++ seq ++ " :"
                                       computeCircular 0 seq
                               else putStrLn $ "\x1b[31m" ++ seq ++ " isn't a deBruijn sequence." ++ "\x1b[0m"

-- Checks if a both sequence are unique or not
uniqueAlgorithm b alp = if checkAlphaUnique alp 0 == False
                       then displayHelp
                       else do seq1 <- getCurrentInput
                               seq2 <- getCurrentInput
                               if isASequence b alp seq1 == True && isASequence b alp seq2 == True
                               then if checkCircular 0 seq1 seq2 == True then putStrLn "KO" else putStrLn "OK"
                                    else putStrLn "KO"

-- Get input while END isn't written
getInput :: [String] -> IO [String]
getInput a = do b <- getCurrentInput
                if b == "END"
                then return a
                else do c <- getInput (a ++ [b]); return c

deleteCircleSequence :: String -> [String] -> Int -> Bool
deleteCircleSequence b a n
                     | n < 0 = True
                     | checkCircular 0 (a!!n) b == True = False
                     | otherwise = deleteCircleSequence b a (n - 1)

pickCorrectSequences :: [String] -> Int -> IO ()
pickCorrectSequences a n
                        | n >= length a = putStr ""
                        | deleteCircleSequence (a!!n) a (n - 1) == True = do putStrLn (a!!n) ; pickCorrectSequences a (n + 1)
                        | otherwise = pickCorrectSequences a (n + 1)

deleteNotSequence :: Int -> String -> [String] -> Int -> [String]
deleteNotSequence b alp a n
                  | n >= length a = a
                  | isASequence b alp (a!!n) == False = deleteNotSequence b alp (delete (a!!n) a) n
                  | otherwise = deleteNotSequence b alp a (n + 1)

cleanAlgorithm b alp = if checkAlphaUnique alp 0 == False
                       then displayHelp
                       else do a <- getInput []
                               pickCorrectSequences (deleteNotSequence b alp a 0) 0

-- Check if alphabet have duplicates characters
checkAlphaUnique :: String -> Int -> Bool
checkAlphaUnique str n
                | n /= length(str) = if numTimesFound (str!!n) str >= 2 then False else checkAlphaUnique str $ n + 1
                | otherwise = True

-- Find number of occurence of all characters in string
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

-- Display help message
displayHelp = do hPutStrLn stderr "USAGE: ./deBruijn n [a] [--check|--unique|--clean|--lyndon|--circular|--read -> file]"
                 hPutStrLn stderr ""
                 hPutStrLn stderr "      --check       check if a sequence is a de Bruijn sequence"
                 hPutStrLn stderr "      --unique      check if 2 sequences are distinct de Bruijn sequences"
                 hPutStrLn stderr "      --clean       list cleaning"
                 hPutStrLn stderr "      --read        read a file with instructions"
                 hPutStrLn stderr "      --lyndon      extract all lyndon words from a sequence"
                 hPutStrLn stderr "      --circular    shows all circular variante of a sequence"
                 hPutStrLn stderr "      n             order of the sequence (> 0)"
                 hPutStrLn stderr "      a             alphabet [def: “01”]"
                 exitWith (ExitFailure 84)
