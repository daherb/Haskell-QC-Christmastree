module Main where

import Tree
import Test.QuickCheck
import Control.Monad
import Data.Functor

main :: IO ()
main = join $ (putStrLn . showTree 10) <$> generate (arbitrary :: Gen Tree)
