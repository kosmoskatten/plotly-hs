{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import System.Environment (getArgs)
import System.Random (randomRIO)

import HBar

main :: IO ()
main = do
  selection <- head <$> getArgs
  case selection of
    "fruit" -> runFruitPopulation
    "gop"   -> runGopPrimarySimulation
    _       -> print ("fruit or gop" :: String)

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

runGopPrimarySimulation :: IO ()
runGopPrimarySimulation =
  runHBar "http://localhost:8888" "GOP primary poll's simulation"
          gopPrimarySimulation

gopPrimarySimulation :: HBar ()
gopPrimarySimulation =
  forever $ do
    state     <- selectFrom states
    candidate <- selectFrom candidates
    vote      <- fromIntegral <$> selectFrom votes

    addItemWithCategory candidate state vote
    commit
    wait

selectFrom :: [a] -> HBar a
selectFrom xs = do
  ind <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! ind

states :: [Text]
states = [ "Arizona", "Ohio", "Oregon", "Florida", "Texas"
         , "Nevada", "North Carolina", "Washington" ]

candidates :: [Text]
candidates = [ "Trump", "Cruz", "Kasich", "Fiorina"
             , "Rubio", "Bush", "Carson" ]

votes :: [Int]
votes = [1..25]

wait :: HBar ()
wait = liftIO $ threadDelay 1000000
