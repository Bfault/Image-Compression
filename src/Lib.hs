--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Lib
--

module Lib
(
    Params (..),
    Cluster (..),
    In (..),
    Point (..),
    Color (..),
    myNth,
    replaceNth,
    unMaybe,
    wordsWhen,
    noneColor,
    noneCluster,
    getDistance,
) where

type X = Int
type Y = Int
type R = Int
type G = Int
type B = Int

data Point = Point 
    {
        _x  :: X,
        _y  :: Y
    } deriving (Show)
data Color = Color
    {
        _r  :: R,
        _g  :: G,
        _b  :: B
    } deriving (Show)
data In = In
    {
        _point  :: Point,
        _color  :: Color
    } deriving (Show)
data Cluster = Cluster
    {
        _colors  :: Color,
        _points  :: [In]
    } deriving (Show)

data Params = Params
    {
        _nbColor    :: Int,
        _limit      :: Float,
        _pathfile   :: String
    } deriving (Show)

myNth :: [a] -> Int -> a
myNth [] _ = error "out of range"
myNth (h:_) 0 = h
myNth (_:t) rank = myNth t (rank - 1)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 newVal (x:xs) = newVal:xs
replaceNth n newVal (x:xs) = x:replaceNth (n-1) newVal xs

unMaybe :: Maybe a -> a
unMaybe (Just a) = a

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen ban sentence = case dropWhile ban sentence of
    "" -> []
    block -> sublock : wordsWhen ban rest
        where
            (sublock, rest) = break ban block

noneColor :: Color
noneColor = Color{_r=0, _g=0, _b=0}

noneCluster :: Cluster
noneCluster = Cluster{_points=[], _colors=noneColor}

getDistance :: Color -> Color -> Float
getDistance Color{_r=r1, _g=g1, _b=b1} Color{_r=r2, _g=g2, _b=b2}
    = sqrt ((r ** 2) + (g ** 2) + (b ** 2))
    where
        r = fromIntegral (r1 - r2)
        g = fromIntegral (g1 - g2)
        b = fromIntegral (b1 - b2)