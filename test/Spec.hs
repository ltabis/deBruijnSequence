import DeBruijn
import Parser
import Data.Text

import Test.Hspec

main :: IO ()
main = hspec $ do

    it "[generateAlgorithm] : simple generation" $ generateAlgorithm 3 "01" [("01"!!0)] `shouldBe` ("00010111" :: String)
    it "[generateAlgorithm] : special alphabet" $ generateAlgorithm 3 "abc" [("abc"!!0)] `shouldBe` ("aaabaacabbabcacbaccbbbcbccc" :: String)
    it "[generateAlgorithm] : big alphabet" $ generateAlgorithm 3 "12345678" [("12345678"!!0)] `shouldBe` ("11121131141151161171181221231241251261271281321331341351361371381421431441451461471481521531541551561571581621631641651661671681721731741751761771781821831841851861871882223224225226227228233234235236237238243244245246247248253254255256257258263264265266267268273274275276277278283284285286287288333433533633733834434534634734835435535635735836436536636736837437537637737838438538638738844454464474484554564574584654664674684754764774784854864874885556557558566567568576577578586587588666766867767868768877787888" :: String)

    it "[deleteNotSequence] : empty sequences" $ deleteNotSequence 5 [] [""] 0 `shouldBe` ([] :: [String])
    it "[deleteNotSequence] : valid and invalid sequences" $ deleteNotSequence 3 "01" ["1000", "0001", "00010111"] 0 `shouldBe` (["00010111"] :: [String])

    it "[checkCircular] : are sircular" $ checkCircular 0 "00101110" "00010111" `shouldBe` (True :: Bool)
    it "[checkCircular] : aren't sircular" $ checkCircular 0 "00101011" "00010111" `shouldBe` (False :: Bool)

    it "[deleteCircleSequence] : empty list" $ deleteCircleSequence "" [""] 0 `shouldBe` (True :: Bool)
    it "[deleteCircleSequence] : duplicate" $ deleteCircleSequence "00010111" ["00101011", "00010111"] 1 `shouldBe` (False :: Bool)

    it "[checkAlphaUnique] : default alphabet" $ checkAlphaUnique "01" 0 `shouldBe` (True :: Bool)
    it "[checkAlphaUnique] : good alphabet" $ checkAlphaUnique "abc" 0 `shouldBe` (True :: Bool)
    it "[checkAlphaUnique] : wrong list" $ checkAlphaUnique "abcdecamd" 0 `shouldBe` (False :: Bool)

    it "[isNum] : is a number - test 0" $ isNum "0" `shouldBe` (0 :: Int)
    it "[isNum] : is a number - test 42" $ isNum "42" `shouldBe` (42 :: Int)
    it "[isNum] : is a number - test 132234343" $ isNum "132234343" `shouldBe` (132234343 :: Int)
    it "[isNum] : is not a number - test abcdecamd" $ isNum "abcdecamd" `shouldBe` ((-1) :: Int)
    it "[isNum] : is not a number - test 1242e" $ isNum "1242e" `shouldBe` ((-1) :: Int)
