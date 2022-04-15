--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Formating
--

module Formating
(
    formatContents,
) where

import Control.Exception
import Error
import Lib
import CheckingArgs
import Text.Read

parserBanner :: Char -> Bool
parserBanner c = c == '(' || c == ')' || c == ','

transformToPoint :: String -> Int
transformToPoint point
    | mresult == Nothing || result < 0 =
        throw (ArgError "Invalid file format")
    | otherwise = result
        where
            mresult = readMaybe point :: Maybe Int
            result = unMaybe mresult

transformToColor :: String -> Int
transformToColor color
    | mresult == Nothing || result < 0 || result > 256 =
        throw (ArgError "Invalid file format")
    | otherwise = result
        where
            mresult = readMaybe color :: Maybe Int
            result = unMaybe mresult

getPoint :: String -> Point
getPoint point = case points of
    Nothing -> throw (ArgError "Invalid color format on input")
    Just point -> Point
        {
            _x=transformToPoint $ myNth point 0,
            _y=transformToPoint $ myNth point 1
        }
    where
        points = chekingLength 2 (wordsWhen parserBanner point)

getColor :: String -> Color
getColor color = case colors of
    Nothing -> throw (ArgError "Invalid point format on input")
    Just color -> Color
        {
            _r=transformToColor $ myNth color 0,
            _g=transformToColor $ myNth color 1,
            _b=transformToColor $ myNth color 2
        }
    where
        colors =  chekingLength 3 (wordsWhen parserBanner color)

formatLine :: String -> In
formatLine content = case a of
    Nothing -> throw (ArgError "Invalid format on input")
    Just a'-> In
        {
            _point=getPoint $ myNth a' 0,
            _color=getColor $ myNth a' 1
        }
    where
        a = chekingLength 2 (wordsWhen (' '==) content)

formatContents :: [String] -> [In]
formatContents = map formatLine