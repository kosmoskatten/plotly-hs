module Main
  ( main
  ) where

import Network.Hive (defaultHiveConfig, hive)

import Server (server)
import Types (newContext)

main :: IO ()
main = do
  context <- newContext "./build"
  hive defaultHiveConfig $ server context
