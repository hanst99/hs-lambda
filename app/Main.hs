{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lambda

main :: IO ()
main = do
  putStrLn "Some example lambda expressions: "
  let succ = Lambda "n" . Lambda "f" . Lambda "x" $ Apply "f" (chainApply ["n", "f", "x"])
  putStrLn $ "succ ::= " ++ pretty succ
  let zero = (Lambda "f" . Lambda "x" $ Variable "x")
  putStrLn $ "zero ::= " ++  pretty zero
  let oneNoncanonical = Apply succ zero
  putStrLn . ("succ zero == "++) . pretty $ oneNoncanonical
  let one = canonicalise oneNoncanonical
  putStrLn . ("one ::= "++) . pretty $ one
  let twoNoncanonical = Apply succ one
  putStrLn . ("succ (succ zero) == "++) . pretty $ twoNoncanonical
  let two = canonicalise twoNoncanonical
  putStrLn . ("two ::= " ++) . pretty $ two
  let plus = Lambda "n" . Lambda "k" . Lambda "f" . Lambda "x" $ chainApply ["n", "f", chainApply ["k", "f", "x"]]
  putStrLn . ("plus ::= " ++) . pretty $ plus
  let noncanonicalFive = chainApply [plus, two, chainApply [plus, two, one]]
  putStrLn . ("plus two (plus two one) == "++) . pretty $ noncanonicalFive
  let five = canonicalise noncanonicalFive
  putStrLn. ("five ::= " ++) . pretty $ five
