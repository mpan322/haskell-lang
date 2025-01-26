{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

prop_length :: Int -> [Int] -> Bool
prop_length x xs = 1 + length xs == length (x:xs)

return []
runTests :: IO Bool
runTests = $quickCheckAll

main = do
  runTests
