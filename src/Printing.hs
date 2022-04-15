--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Printing
--

module Printing
(
    printHelp,
    printOutput,
) where

import Lib
import System.Exit

printIn :: [In] -> IO ()
printIn [] = return ()
printIn (In{_point=point, _color=color} : rest) = (putStrLn $
    "(" ++ show x ++ "," ++ show y ++ ") "
    ++ "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")
    >> printIn rest
    where
        x = _x point
        y = _y point
        r = _r color
        g = _g color
        b = _b color

printColor :: Color -> IO ()
printColor Color{_r=r, _g=g, _b=b} = putStrLn $
    "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

printOutput :: [Cluster] -> IO ()
printOutput [] = return ()
printOutput (cluster:clusters) =
    putStrLn "--"
    >> printColor (_colors cluster)
    >> putStrLn "-"
    >> printIn (_points cluster)
    >> printOutput clusters

printHelp :: [String] -> IO ()
printHelp (('-' : '-' : 'h' : 'e' : 'l' : 'p' : _) : _) =
    putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n" >>
    putStrLn "\tN\tnumber of colors in the final image" >>
    putStrLn "\tL\tconvergence limit" >>
    putStrLn "\tF\tpath to the file containing the colors of the pixels" >>
    exitSuccess
printHelp _ = return ()