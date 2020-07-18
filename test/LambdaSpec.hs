{-# LANGUAGE OverloadedStrings #-}
module LambdaSpec(spec)
where

import Lambda
import Test.Hspec

spec :: Spec
spec = do
  let zero = Lambda "f" . Lambda "x" $ "x"
      succ = Lambda "n" . Lambda "f" . Lambda "x" $ chainApply ["f", chainApply ["n", "f", "x"]]
      nonCanonicalOne = Apply succ zero
      canonicalOne = Lambda "f" . Lambda "x" $ Apply "f" "x"
      plus = Lambda "n" . Lambda "k" . Lambda "f" . Lambda "x" $ chainApply ["n", "f", chainApply ["k", "f", "x"]]
      canonicalTwo = Lambda "f" . Lambda "x" $ Apply "f" (Apply "f" "x")
      canonicalFive = Lambda "f" . Lambda "x" $ Apply "f" (Apply "f" (Apply "f" (Apply "f" (Apply "f" "x"))))

  describe "canonicalise" $ do
    it "should map zero to itself (it is already canonical)" $
      canonicalise zero `shouldBe` zero
    it "should map succ to itself (it is already canonical)" $
      canonicalise succ `shouldBe` succ
    it "should map succ zero to a canonical one" $ do
      -- first sanity check
      nonCanonicalOne `shouldSatisfy` (/= canonicalOne)
      canonicalise nonCanonicalOne `shouldBe` canonicalOne
    it "should map a canonical one to itself" $
      canonicalise canonicalOne `shouldBe` canonicalOne
    it "should map plus to itself" $
      canonicalise plus `shouldBe` plus
    it "should map (plus one one) to canonical two" $
      canonicalise (chainApply [plus, canonicalOne, canonicalOne]) `shouldBe` canonicalTwo
    it "should map (plus (plus one two) two) to canonical five" $
      canonicalise (chainApply [plus, chainApply [plus, canonicalOne, canonicalTwo], canonicalTwo]) `shouldBe` canonicalFive

  describe "pretty" $ do
    it "should pretty print zero" $
      pretty zero `shouldBe` "λf. λx. x"
    it "should pretty print succ" $
      pretty succ `shouldBe` "λn. λf. λx. f ((n f) x)"
    it "should pretty print five" $
      pretty canonicalFive `shouldBe` "λf. λx. f (f (f (f (f x))))"
