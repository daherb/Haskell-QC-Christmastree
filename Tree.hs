module Tree where

import Test.QuickCheck

type Level = [Deko]

data Tree = T Int [Level]

data Deko = Empty | Lametta | Ball deriving (Show,Eq)

step = 3

instance Arbitrary Tree where
  arbitrary =
    do
      height <- choose (10,20)
      lametta_ratio <- choose (10,50)
      ball_ratio <- choose (20,30)
      createTree height lametta_ratio ball_ratio

randomDeko :: Int -> Int -> Int -> Gen Deko
randomDeko lametta_ratio ball_ratio empty_ratio =
  frequency [(lametta_ratio,return Lametta),(ball_ratio, return Ball),(empty_ratio, return Empty)]
  
instance Arbitrary Deko where
  arbitrary =
    do
      empty_ratio <- choose (0,100)
      lametta_ratio <- choose (0,100 - empty_ratio)
      ball_ratio <- choose (0,100 - lametta_ratio - empty_ratio)
      randomDeko lametta_ratio ball_ratio empty_ratio

createTree :: Int -> Int -> Int -> Gen Tree
createTree height lametta_ratio ball_ratio =
  let
    createTree' :: Int -> Int -> Int -> Int -> Gen [[Deko]]
    createTree' height level lametta_ratio ball_ratio
      | height == level = return []
      | otherwise =
        do
          current_level <- infiniteListOf $ randomDeko lametta_ratio ball_ratio (100 - lametta_ratio - ball_ratio)
          next_levels <- createTree' height (level + 1) lametta_ratio ball_ratio
          return $ take (level * 2 - (2 * level `div` step)) current_level:next_levels
  in
    do
      levels <- createTree' height 0 lametta_ratio ball_ratio
      return $ T height levels


showTree :: Int -> Tree -> String
showTree indent (T height levels) =
  let
    showDeko :: Deko -> Char
    showDeko Ball = 'o'
    showDeko Lametta = '|'
    showDeko Empty = ' '
    showLevel :: Int -> Int -> [Deko] -> String
    showLevel height level dekos = replicate (indent + height - level + (level `div` step)) ' ' ++ '/':map showDeko dekos ++ "\\"
  in
    unlines $ zipWith (showLevel height) [0..height] levels

instance Show Tree where
  show = showTree 0
