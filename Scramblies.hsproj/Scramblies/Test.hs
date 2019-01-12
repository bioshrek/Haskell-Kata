module Test where

import Scramblies (scramble)   

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScramble :: [Char] -> [Char] -> Bool -> Spec
testScramble s1 s2 s = 
    it (printf "should return scramble for s1: %s, s2: %s " s1 s2) $
        scramble s1 s2 `shouldBe` s

main = hspec $ do
 
    describe "scramble: Basic Tests" $ do 
        testScramble "rkqodlw" "world" True
        testScramble "cedewaraaossoqqyt" "codewars" True
        testScramble "katas" "steak" False
        testScramble "scriptjavx" "javascript" False
        testScramble "scriptingjava" "javascript" True
        testScramble "scriptsjava" "javascripts" True
        testScramble "javscripts" "javascript" False
        testScramble "aabbcamaomsccdd" "commas" True
        testScramble "commas" "commas" True
        testScramble "sammoc" "commas" True