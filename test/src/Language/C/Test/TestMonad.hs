{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestMonad
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (MPTC, GeneralizedNewtypeDeriving)
--
-- Monad for testing, providing error continuation, state and IO.
-----------------------------------------------------------------------------
module Language.C.Test.TestMonad (
-- * Temporary test data
TestData(..),emptyTestData,cleanTmpFiles,initTestData,setTestRunResults,addTest,addTmpFile,setTmpTemplate,setTestExit,
-- * test monad
TestMonad,runTests,
-- * actions in the test monad
dbgMsg,addTestM,liftIOCatched,exitTest,errorOnInit,time,withTempFile_,withTempFile,
-- * main convenience
defaultMain,
)
where
import Control.Applicative (Applicative (..))
import Control.Exception (catch)
import Control.Monad
import Control.Monad.Cont (MonadCont, ContT, runContT, callCC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local, ask)
import Control.Monad.State (MonadState, StateT, execStateT, put, get, modify, gets)
import Data.Map (elems)
import Prelude hiding (Applicative(..))
import System.CPUTime
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO (openTempFile,hClose,hPutStrLn,Handle,stderr)
import Text.PrettyPrint

import Language.C.Test.Framework
import Language.C.Test.Environment

-- =======================
-- = Temporary Test data =
-- =======================

-- | Test intermediate data
data TestData = TestData
  {
    tempFiles :: [FilePath],
    tmpTemplate :: String,
    testExit  :: TestMonad (),
    runResults :: TestRun
  }
emptyTestData :: TestData
emptyTestData = TestData [] "parse-test" (error "testExit : undefined") (error "runResults :: undefined")
cleanTmpFiles :: TestData -> IO ()
cleanTmpFiles td = mapM_ removeFile (tempFiles td)
initTestData :: TestRun -> TestData
initTestData runsInit = emptyTestData { runResults = runsInit }
setTestRunResults :: TestRun -> TestData -> TestData
setTestRunResults tr testData = testData { runResults = tr }
addTest :: TestResult -> (TestData -> TestData)
addTest result testData = testData { runResults = insertTest result (runResults testData) }
addTmpFile :: FilePath -> (TestData -> TestData)
addTmpFile tmpFile testData = testData { tempFiles = (tmpFile : tempFiles testData) }
setTmpTemplate :: String -> (TestData -> TestData)
setTmpTemplate tmpl testData = testData { tmpTemplate = tmpl }
setTestExit :: TestMonad () -> (TestData -> TestData)
setTestExit exit testData = testData { testExit = exit }
-- ==============
-- = Test Monad =
-- ==============
newtype TestMonad a = TM { unTM :: ContT () (ReaderT TestConfig (StateT TestData IO)) a } deriving (Monad, Applicative, Functor)
instance MonadReader TestConfig TestMonad where
  ask = TM ask
  local f = TM . local f . unTM
instance MonadState TestData TestMonad where
  get = TM get
  put = TM . put
-- wrap-unwrap-wrap, but no guacamole
instance MonadCont TestMonad where
  callCC cc = TM $ callCC (\continuation -> unTM (cc (TM . continuation)))
instance MonadIO TestMonad where
  liftIO = TM . liftIO

runTests :: TestConfig -> TestMonad () -> IO TestData
runTests config = (flip execStateT) emptyTestData . (flip runReaderT) config . (flip runContT) return . unTM

-- ======================
-- = Test monad actions =
-- ======================
dbgMsg :: String -> TestMonad ()
dbgMsg msg = liftM debug ask >>= \dbg -> liftIO (dbg msg)

addTestM :: TestResult -> TestMonad ()
addTestM result = do
  config <- ask
  modify $ addTest result
  liftIO $ logger config $ show (pretty result) ++ "\n"

time :: TestMonad a -> TestMonad (a, Time)
time action = do
  start <- liftIO $ getCPUTime
  r     <- action
  end   <- liftIO $ getCPUTime
  let durSecs = picoSeconds (end - start)
  return (r, durSecs)

liftIOCatched :: IO a -> TestMonad (Either IOError a)
liftIOCatched a = liftIO $ liftM Right a `catch` (return . Left)

errorOnInit :: [String] -> String -> TestMonad a
errorOnInit args msg = do
  config <- ask
  liftIO $ debug config $ "Failed to initialize " ++ msg ++ "\n"
  liftIO $ logger config $ show (initFailure msg args)
  modify (setTestRunResults (initFailure msg args))
  exitTest

exitTest :: TestMonad a
exitTest = join (gets testExit) >> error "Internal call/cc error (not reached)"

withTempFile_ :: String -> (Handle -> TestMonad a) -> TestMonad FilePath
withTempFile_ ext = liftM fst . withTempFile ext

withTempFile :: String -> (Handle -> TestMonad a) -> TestMonad (FilePath, a)
withTempFile ext a = do
  tmpdir <- liftM tmpDir ask
  tmpl   <- gets tmpTemplate
  (tmpFile, tmpHnd) <- liftIO $ openTempFile tmpdir (tmpl ++ ext)
  r <- a tmpHnd
  liftIO$ hClose tmpHnd
  return (tmpFile, r)


-- | @defaultMain testRunner executes the test set @tests = testRunner cmdLineArgs@
--   and records the results, using the provided environment variables (see 'TestEnvironment').
defaultMain :: Doc -> ([String] -> TestMonad ()) -> IO ()
defaultMain usage testRunner = do
  -- get arguments
  args <- getArgs
  when (null args) $ do
    hPutStrLn stderr "! No arguments given\n"
    hPutStrLn stderr ("Usage:\n"++show usage)
    exitWith $ ExitFailure 1

  -- read environment vars
  (config,resultFile) <- getEnvConfig

  -- execute tests
  testDat <- runTests config $ callCC $ \cc -> do
    modify $ setTestExit $ cc ()
    testRunner args

  -- write results
  debug config $ "Finished test\n"

  appendFile resultFile (show (runResults testDat) ++ "\n")

  debug config $ "Wrote test results. Cleaning up.\n"

  when (not $ keepIntermediate config) $ cleanTmpFiles testDat

  -- for regression testing
  exit_failure <- getEnvFlag exitFailureEnvVar
  when exit_failure $ do
    case runResults testDat of
      TestResults _ _ results -> do
        when (any (not . isTestOk . testStatus) (Data.Map.elems results)) $
          exitWith (ExitFailure 1)
      _ -> exitWith (ExitFailure 2)

