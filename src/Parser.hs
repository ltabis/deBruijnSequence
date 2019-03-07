module Parser where

import System.Environment
import System.Exit
import System.IO
import Text.Read
import DeBruijn
import ReadFile

-- get args
processArgs :: IO ([String])
processArgs = do
            a <- getArgs
            return a

-- Convert string to int or return -1
isNum :: String -> Int
isNum a = case readMaybe a :: Maybe Int of
          Just a -> a
          Nothing -> (-1)

-- Compare all arguments / A TERMINER
compareArguments :: [String] -> Int -> IO ()
compareArguments a b
                 | length (a) == 4 && (a!!2) == "--read" = if b > 0 then readMyFile b (a!!1) (a!!3) else displayHelp
                 | length (a) == 3 && (a!!1) == "--read" = if b > 0 then readMyFile b "01" (a!!2) else displayHelp
                 | length (a) == 1 = if b > 0 then postAlgorithmProcess b "01" else displayHelp
                 | length (a) == 2 && (a!!1) /= "--check" && (a!!1) /= "--lyndon" && (a!!1) /= "--circular" && (a!!1) /= "--unique" && (a!!1) /= "--clean" = if b /= (-1) then postAlgorithmProcess b (a!!1) else do displayHelp; exitWith (ExitFailure 84)
                 | length (a) == 3 = if b > 0 then parseOptions b (a!!1) (a!!2) else displayHelp
                 | otherwise = if b > 0 then parseOptions b "01" (a!!1) else displayHelp


parseOptions :: Int -> String -> String -> IO ()
parseOptions b alp "--check" = if length alp == 0 then displayHelp else checkAlgorithm b alp
parseOptions b alp "--unique" = if length alp == 0 then displayHelp else uniqueAlgorithm b alp
parseOptions b alp "--clean" = if length alp == 0 then displayHelp else cleanAlgorithm b alp
parseOptions b alp "--lyndon" = if length alp == 0 then displayHelp else lyndonAlgorithm b alp
parseOptions b alp "--circular" = if length alp == 0 then displayHelp else circularAlgorithm b alp
parseOptions b alp x = displayHelp
