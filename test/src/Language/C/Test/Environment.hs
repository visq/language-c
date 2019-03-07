{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Environment
-- Copyright   :  (c) 2008 Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides access to the environment variables used for testing, and provides
-- them in form of a TestConfig datum.
--
-- `envHelpDoc' says:
--
-- Influental environment variables:
--     CTEST_TMPDIR                  the temporary directory to write test reports to
--     CTEST_LOGFILE                 the log file [default = <stderr>]
--     CTEST_REPORT_FILE             file to write test reports to [default = CTEST_TMPDIR/report.dat]
--     CTEST_DEBUG                   whether to print debug messages [default = False]
--     CTEST_KEEP_INTERMEDIATE       whether to keep intermediate files [default = False]
--     CTEST_EXIT_FAILURE            whether to exit with failure if one test fails
-----------------------------------------------------------------------------
module Language.C.Test.Environment (
-- * Test Configurations
TestConfig(..),
envHelpDoc,
getEnvFlag,exitFailureEnvVar,
getEnvConfig,
-- * Process cpp arguments
isPreprocessedFile,MungeResult(..),mungeCcArgs,
)
where
import Control.Exception (catch)
import Control.Monad (liftM)
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import System.Environment
import System.FilePath (combine)
import System.IO
import Text.PrettyPrint
#if (MIN_VERSION_base(4, 11, 0))
import Prelude hiding ((<>))
#endif

-- | Takes a list of additional environment variable descriptions, and produces a document providing help
--   on the influental environment variables.
envHelpDoc :: [(String, (String, Maybe String ))] -> Doc
envHelpDoc addEnvHelp
  = text "Influental environment variables:" $+$
    (nest 4 . vcat . map varInfo) (envHelp ++ addEnvHelp)
  where
    varInfo (var, (descr, def)) = text var $$ nest 30 (prettyDescr descr def)
    prettyDescr descr def = text descr <> prettyDef def
    prettyDef Nothing = empty
    prettyDef (Just def) = text $ " [default = " ++ def ++ "]"

envHelp :: [(String, (String, Maybe String))]
envHelp = [ vi tmpdirEnvVar "the temporary directory to write test reports to" Nothing,
            vi logfileEnvVar "the log file" (Just "<stderr>"),
            vi reportFileEnvVar "file to write test reports to" (Just (tmpdirEnvVar++"/report.dat")),
            vi debugEnvVar "whether to print debug messages" (Just ("False")),
            vi keepImEnvVar "whether to keep intermediate files" (Just ("False")),
            vi exitFailureEnvVar "whether to exit with failure if one test fails" (Just ("False"))
          ]
  where vi envVar varDescr varDef = (envVar, (varDescr, varDef))

data TestConfig = TestConfig
  {
    debug :: String -> IO (),
    logger :: String -> IO (),
    keepIntermediate :: Bool,
    tmpDir :: FilePath
  }

tmpdirEnvVar :: String
tmpdirEnvVar = "CTEST_TMPDIR"
logfileEnvVar :: String
logfileEnvVar = "CTEST_LOGFILE"
reportFileEnvVar :: String
reportFileEnvVar = "CTEST_REPORT_FILE"
debugEnvVar :: String
debugEnvVar = "CTEST_DEBUG"
keepImEnvVar :: String
keepImEnvVar = "CTEST_KEEP_INTERMEDIATE"
exitFailureEnvVar :: String
exitFailureEnvVar = "CTEST_EXIT_FAILURE"
defaultReportFile :: String
defaultReportFile = "report.dat"

getEnvConfig :: IO (TestConfig, FilePath)
getEnvConfig = do
  environ <- liftM Map.fromList getEnvironment
  -- get log dir, result file and debug flag
  tmpdir  <- getEnv tmpdirEnvVar
  let resultFile = maybe (combine tmpdir defaultReportFile) id $ Map.lookup reportFileEnvVar environ
  let debugFlag  = maybe False (const True)   $ Map.lookup debugEnvVar environ
  let keepIm     = maybe False (const True)   $ Map.lookup keepImEnvVar environ
  let logFile    = maybe Nothing Just         $ Map.lookup logfileEnvVar environ
  let config = TestConfig {
                    debug  = debugAction debugFlag,
                    logger = logAction logFile,
                    tmpDir = tmpdir,
                    keepIntermediate = keepIm
                  }
  return (config,resultFile)
  where
    debugAction False = \_ -> return ()
    debugAction True  = hPutStr stderr . ("[DEBUG] "++)
    logAction   Nothing = hPutStr stderr
    logAction   (Just logFile) = appendFile logFile

getEnvFlag :: String -> IO Bool
getEnvFlag envVar = do
  envFlag <- getEnv envVar `catch` (\e -> return ((e :: IOError) `seq` "0"))
  return $ case envFlag of
    _ | envFlag == "" || envFlag == "0" || "f" `isPrefixOf` (map toLower envFlag) -> False
      | otherwise                                                                 -> True

isPreprocessedFile :: String -> Bool
isPreprocessedFile = (".i" `isSuffixOf`)

data MungeResult = Unknown String | Ignore | Groked [FilePath] [String]

mungeCcArgs :: [String] -> MungeResult
mungeCcArgs = mungeArgs [] []

mungeArgs :: [String] -> [String] -> [String] -> MungeResult
mungeArgs _accum []    [] = Unknown "No .c / .hc / .i source file given"
mungeArgs accum cfiles [] = Groked cfiles (reverse accum)
-- ignore preprocessing - only calls
mungeArgs _accum _cfiles ("-E":_) = Ignore
-- ignore make-rule creation
mungeArgs _accum _cfiles ("-M":_) = Ignore
-- strip outfile
mungeArgs accum cfiles ("-o":_outfile:args) = mungeArgs accum cfiles args
mungeArgs accum cfiles (cfile':args)
          | ".c" `isSuffixOf` cfile'
         || ".hc" `isSuffixOf` cfile'
         || ".i"  `isSuffixOf` cfile' =
              mungeArgs accum (cfile':cfiles) args
mungeArgs _accum _cfiles (cfile':_)
          | ".S" `isSuffixOf` cfile' = Ignore
mungeArgs accum cfiles (arg:args) = mungeArgs (arg:accum) cfiles args
