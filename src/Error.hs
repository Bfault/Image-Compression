--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Error
--

module Error
(
    handleException,
    handleOpen,
    ExceptionType (..),
) where

import Control.Exception
import System.Exit
import System.IO ( hPutStrLn, stderr )

data ExceptionType = ArgError String deriving (Show)

instance Exception ExceptionType

printErrorAndExit :: String -> IO ()
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

handleException :: ExceptionType -> IO ()
handleException (ArgError msg) = printErrorAndExit $ "Error: " ++ msg

handleOpen :: IOException -> IO ()
handleOpen _ = throw (ArgError "file is not good")