{-# LANGUAGE OverloadedStrings #-}
module Main where

import HBar

main :: IO ()
main = runFruitPopulation

runFruitPopulation :: IO ()
runFruitPopulation =
  runHBar "http://localhost:8888" "Fruit popularity by country"
          fruitPopulation

fruitPopulation :: HBar ()
fruitPopulation = do
  addItemWithCategory "Apple" "Sweden" 88
  addItemWithCategory "Orange" "Sweden" 110
  addItemWithCategory "Banana" "Sweden" 23

  addItemWithCategory "Apple" "Norway" 67
  addItemWithCategory "Orange" "Norway" 15
  addItemWithCategory "Banana" "Norway" 90

  addItemWithCategory "Pineapple" "Denmark" 45
  addItemWithCategory "Apple" "Denmark" 19
  addItemWithCategory "Orange" "Denmark" 46
  addItemWithCategory "Banana" "Denmark" 8

  commit
