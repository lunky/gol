module Lib
    (  tick
      ,lives
      ,printGameState
      ,GameState(..)
      ,Point(..)
    ) where

import Data.List (nub,groupBy)

newtype GameState = GameState [Point] deriving (Show,Eq)
newtype Point = Point (Int,Int) deriving (Show,Eq,Ord)

adjacent :: Point -> [Point]
adjacent (Point (x,y)) = [
  Point (x-1,y-1),Point (x,y-1),Point (x+1,y-1),
  Point (x-1,y),                Point (x+1,  y),
  Point (x-1,y+1),Point (x,y+1),Point (x+1,y+1) ]

lives :: Point -> GameState -> Bool
lives point (GameState state)
    | currentlyLiving && (liveNeighbors ==2 || liveNeighbors == 3) = True
    | not currentlyLiving && liveNeighbors ==3 = True
    | otherwise = False
    where neighbors = adjacent point
          liveNeighbors = length $ filter (`elem` state) neighbors
          currentlyLiving = point `elem` state

tick :: GameState -> GameState
tick (GameState gamestate) = GameState (filter (\y -> lives y (GameState gamestate)) gameStateAndAdjacent)
  where gameStateAndAdjacent = nub $ gamestate ++ concatMap adjacent gamestate

printGameState :: GameState -> IO()
printGameState (GameState state) =  mapM_ 
                                  (putStrLn . map (\y->if Point y `elem`  state then '#' else '.')) 
                                  (groupBy (\x y->fst x==fst y) fullRange)
  where fullRange = [(x,y) | x <- [minXY..maxXY], y<- [minXY..maxXY]]
        minXY = minimum $ concatMap (\(Point (i,j))->[i,j]) state
        maxXY = maximum $ concatMap (\(Point (i,j))->[i,j]) state

