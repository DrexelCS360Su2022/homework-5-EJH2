module HaskellIntroSpec (
    main,
    spec
  ) where

import Math.OEIS (getSequenceByID)
import Test.Hspec
import Test.QuickCheck

import HaskellIntro
import Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 1: Implementing the Luhn Algorithm" $ do
    it "lastDigit 123 == 3" $ do
        lastDigit 123 `shouldBe` 3
    it "lastDigit 0 == 0" $
      lastDigit 0 `shouldBe` 0
    it "dropLastDigit 123 == 12" $
      dropLastDigit 123 `shouldBe` 12
    it "dropLastDigit 5 == 0" $
      dropLastDigit 5 `shouldBe` 0
    it "toDigits 1234 == [1,2,3,4]" $
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "toDigits 0 == []" $
      toDigits 0 `shouldBe` []
    it "toDigits (-17) == []" $
      toDigits (-17) `shouldBe` []
    it "doubleEveryOther [8,7,6,5] == [16,7,12,5]" $
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    it "doubleEveryOther [1,2,3] == [1,4,3]" $
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
    it "sumDigits [] == 0" $
      sumDigits [] `shouldBe` 0
    it "sumDigits [0] == 0" $
      sumDigits [0] `shouldBe` 0
    it "sumDigits [16,7,12,5] == 22" $
      sumDigits [16,7,12,5] `shouldBe` 22
    it "sumDigits [27] == 9" $
      sumDigits [27] `shouldBe` 9
    it "validate 4012888888881881 = True" $
      validate 4012888888881881 `shouldBe` True
    it "validate 4012888888881882 = False" $
      validate 4012888888881882 `shouldBe` False
  describe "Problem 2: Hofstadter Sequences" $ do
    it "pow square 0" $
      let square :: Integer -> Integer
          square x = x * x
      in
        forAll arbitrary $ \x -> pow square 0 x === x
    it "pow square 2" $
      let square :: Integer -> Integer
          square x = x * x
      in
        forAll arbitrary $ \x -> pow square 2 x === x*x*x*x
    it "G sequence correct" $
      testOEIS "A005206" (map g [0..])
    it "H sequence correct" $
      testOEIS "A005374" (map h [0..])
    it "D_2 sequence correct" $
      testOEIS "A005206" (map (d 2) [0..])
    it "D_3 sequence correct" $
      testOEIS "A005374" (map (d 3) [0..])
  describe "Problem 3: Power set" $ do
    it "PowerSet of empty set" $
      powerSet (empty :: Set Int) `shouldBe` (singleton empty :: Set (Set Int))
    it "PowerSet of singleton set" $
      powerSet (singleton (1 :: Int)) `shouldBe`fromList [empty, singleton 1]
    it "PowerSet of {1,2,3}" $
      powerSet s `shouldBe` p
  where
    a, b, c :: String
    a = "x"
    b = "y"
    c = "z"

    s :: Set Int
    s = fromList [1,2,3]

    p :: Set (Set Int)
    p = fromList [empty, singleton 1, singleton 2, singleton 3
                 ,fromList [1,2], fromList [1,3], fromList [2,3]
                 ,s]

    testOEIS :: String -> [Integer] -> Bool
    testOEIS oeid xs =
        take (length ys) xs == ys
      where
        Just ys = getSequenceByID oeid
