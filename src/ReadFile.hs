module ReadFile where

import System.IO
import System.Exit

import DeBruijn

-- Check alphabet before testing what parameter is entered
postAlgorithmProcess :: Int -> String -> IO ()
postAlgorithmProcess n str = if checkAlphaUnique str 0 == False || length str == 0
                          then displayHelp
                          else putStrLn $ generateAlgorithm n str [(str!!0)]

-- enter
readMyFile :: Int -> String -> String -> IO ()
readMyFile b alp file = do handle <- openFile file ReadMode
                           readMyLine handle b alp

-- read line of a file
readInput :: Handle -> IO String
readInput file = do a <- hIsEOF file
                    if a
                    then exitWith ExitSuccess
                    else do a <- hGetLine file
                            return a

-- read by block
readMyLine :: Handle -> Int -> String -> IO ()
readMyLine file b alp = do a <- readInput file
                           if a == "CHECK" || a == "UNIQUE" || a == "CLEAR" || a == "GENERATE" || a == "LYNDON" || a == "CIRCULAR"
                           then do parseBlock a file b alp ; readMyLine file b alp
                           else readMyLine file b alp

-- get input in file
getFileInput :: [String] -> Handle -> IO [String]
getFileInput a file = do b <- readInput file
                         if b == "END"
                         then return a
                         else do c <- getFileInput (a ++ [b]) file; return c

-- parse file when CHECK line
parseBlock :: String -> Handle -> Int -> String -> IO ()
parseBlock "GENERATE" file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "GENERATE"
                                   else do putStrLn $ "\x1b[34m" ++ ("\nGenerating a sequence with words of len " ++ show (b) ++ (" and the '" ++ alp) ++ "' alphabet.") ++ "\x1b[0m"
                                           postAlgorithmProcess b alp

parseBlock "CHECK"    file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "CHECK"
                                   else do a <- readInput file
                                           putStrLn $ "\x1b[34m" ++ ("\nTesting if " ++ a) ++ (" is a real deBruijn sequence") ++ "\x1b[0m"
                                           if isASequence b alp a == True
                                           then putStrLn $ "\x1b[32m" ++ a ++ " is a sequence." ++ "\x1b[0m"
                                           else putStrLn $ "\x1b[31m" ++ a ++ " isn't a deBruijn sequence." ++ "\x1b[0m"

parseBlock "UNIQUE"   file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "UNIQUE"
                                   else do a <- readInput file
                                           c <- readInput file
                                           putStrLn $ "\x1b[34m" ++ ("\nTesting unique between " ++ a) ++ (" and " ++ c) ++ "." ++ "\x1b[0m"
                                           if isASequence b alp a == True
                                           then if isASequence b alp c == True
                                                 then if checkCircular 0 a c == True
                                                      then putStrLn $ "\x1b[31m" ++ (a ++ " and ") ++ (c ++ " aren't unique.") ++ "\x1b[0m"
                                                      else putStrLn $ "\x1b[32m" ++ (a ++ " and ") ++ (c ++ " are both unique.") ++ "\x1b[0m"
                                                 else putStrLn $ "\x1b[31m" ++ c ++ " isn't a deBruijn sequence." ++ "\x1b[0m"
                                           else putStrLn $ "\x1b[31m" ++ a ++ " isn't a deBruijn sequence." ++ "\x1b[0m"

parseBlock "CLEAR"    file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "CLEAR"
                                   else do a <- getFileInput [] file
                                           putStrLn $ "\x1b[34m" ++ "\nSequences to clean : " ++ (show $ length a)
                                           putStrLn $ "Cleaning sequences ..." ++ "\x1b[0m"
                                           pickCorrectSequences (deleteNotSequence b alp a 0) 0
                                           putStrLn $ "\x1b[34m" ++ "End of sequences cleaned." ++ "\x1b[0m"

parseBlock "LYNDON"    file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "LYNDON"
                                   else do a <- readInput file
                                           if isASequence b alp a == True
                                           then do putStrLn $ "\x1b[34m" ++ "\nAll combinations of Lyndon words in " ++ a ++ " :" ++ "\x1b[0m"
                                                   print $ getAllCombinations b a a [] 0
                                           else putStrLn $ "\x1b[31m" ++ a ++ " isn't a deBruijn sequence." ++ "\x1b[0m"

parseBlock "CIRCULAR"  file b alp = if checkAlphaUnique alp 0 == False
                                   then displayParsingError "CIRCULAR"
                                   else do a <- readInput file
                                           if isASequence b alp a == True
                                           then do putStrLn $ "\x1b[34m" ++ "\nAll circular variations of " ++ a ++ " :" ++ "\x1b[0m"
                                                   computeCircular 0 a
                                           else putStrLn $ "\x1b[31m" ++ a ++ " isn't a deBruijn sequence." ++ "\x1b[0m"


parseBlock line file b alp = displayParsingError line

displayParsingError :: String -> IO ()
displayParsingError line = do hPutStrLn stderr  $ "\x1b[31m" ++ "[ PARSING ERROR ]"
                              hPutStrLn stderr ""
                              hPutStrLn stderr $ "At line [" ++ line ++ "]" ++ "\x1b[0m"
                              exitWith (ExitFailure 84)
