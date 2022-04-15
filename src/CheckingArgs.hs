--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- CheckingArgs
--

module CheckingArgs
(
    checkingArgs,
    chekingLength,
    checkingInput,
) where

import Lib
import Error
import Text.Read
import Control.Exception

chekingLength :: Int -> [a] -> Maybe [a]
chekingLength len list
    | length list == len = Just list
    | otherwise = Nothing

checkingInput :: Params -> [String] -> IO ()
checkingInput Params{_nbColor=nbColor} list
    | nbColor > length list = throw (ArgError "-n must be too high")
    | otherwise = return ()

fillNbColor :: Params -> Maybe Int -> Params
fillNbColor _ Nothing = throw (ArgError "-n must be a integer")
fillNbColor param (Just nb)
    | nb < 0 = throw (ArgError "-n must be positive")
    | otherwise = param{_nbColor=nb}

fillLimit :: Params -> Maybe Float -> Params
fillLimit _ Nothing = throw (ArgError "-l must be a floating value")
fillLimit param (Just nb)
    | nb < 0 = throw (ArgError "-l must be positive")
    | otherwise = param{_limit=nb}

fillPathfile :: Params -> String -> Params
fillPathfile param name = param{_pathfile=name}

fillArgs :: [String] -> Params -> Params
fillArgs [] param = param
fillArgs ("-n": nb: list) param =
    fillArgs list (fillNbColor param (readMaybe nb))
fillArgs ("-l": nb: list) param =
    fillArgs list (fillLimit param (readMaybe nb :: Maybe Float))
fillArgs ("-f": name: list) param =
    fillArgs list (fillPathfile param name)
fillArgs (p: _) _ = throw (ArgError $ p ++ " Parameter not know")

checkingArgs :: [String] -> Params -> Params
checkingArgs args param
    | (/=) (length args) 6 = throw (ArgError "Invalid number of parameters")
    | otherwise = fillArgs args param