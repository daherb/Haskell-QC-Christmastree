module Main where

import Tree
import Test.QuickCheck
import Control.Monad
import Data.Functor

main :: IO ()
main = join $ (putStrLn . show) <$> generate (arbitrary :: Gen Tree)
