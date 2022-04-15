--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Main
--

module Main where

import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitSuccess), exitWith, exitSuccess )
import Control.Exception ( handle )
import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import System.Random ( Random(randomIO) )
import Control.Monad ( replicateM )

import Error
import CheckingArgs
import Lib
import Formating
import Compute
import Printing

initParams :: Params
initParams = Params
    {
    _nbColor= -1,
    _limit= -1.0,
    _pathfile="null"
    }

submain :: Params -> IO ()
submain param@Params{_pathfile=pathfile, _nbColor=nbColor} =
    handle handleOpen $ do
        grip <- openFile pathfile ReadMode
        contents <- hGetContents grip
        let rcontents = wordsWhen (=='\n') contents
        let fcontents = formatContents rcontents
        checkingInput param rcontents
        rdList <- replicateM nbColor (randomIO :: IO Int)
        printOutput $ findCluster param fcontents rdList
        hClose grip
        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    printHelp args
    handle handleException (submain $ checkingArgs args initParams)
