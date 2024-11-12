-----------------------------------------------------------------------------
-- |
-- Module      :  TestFramework
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides a small framework for testing IO - based stuff.
-------------------------------------------------------------------------------------------------------
module Language.C.Test.Framework (
-- * Test descriptions
Test(..),testTemplate,
-- * Test results
TestResult(..),initializeTestResult,setTestStatus,
-- * Status of a test
TestStatus(..),testError,isTestError,
testFailure,testFailNoReport,testFailWithReport,
testOk,testOkNoReport,testOkWithReport,testOkUntimed,isTestOk,
-- * Test runs, i.e. a sequence of consecutive (usually dependent) tests of a single test object
TestRun(..),hasTestResults,initFailure,emptyTestResults,insertTest,
-- ReExport pretty from the Language.C library
Pretty(..),
-- ReExport the formatting stuff
module Language.C.Test.Measures,
)
where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import Language.C.Pretty
import Language.C.Test.Measures

-- =====================
-- = Test descriptions =
-- =====================
data Test = Test
  {
    testName :: String,
    testDescr :: String,
    preferredScale :: MetricScale,
    inputUnit :: UnitDescr
  }
  deriving (Show,Read)

testTemplate :: String -> String -> MetricScale -> UnitDescr -> Test
testTemplate testname testdescr preferredscale inputdim =
  Test testname testdescr preferredscale inputdim

-- ================
-- = Test results =
-- ================

-- | Result of a test
data TestResult =
  TestResult {
    testInfo :: Test,
    testArgs :: [String],
    testStatus :: TestStatus
  }
  deriving (Show,Read)

initializeTestResult :: Test -> [String] -> TestResult
initializeTestResult t args = TestResult t args (testError "not exectued")

setTestStatus :: TestResult -> TestStatus -> TestResult
setTestStatus testresult status = testresult { testStatus = status }

-- | Status of a test
data TestStatus =
    TestError String
  | TestFailure String (Maybe FilePath)
  | TestOk (Maybe PerfMeasure) (Maybe FilePath)
  deriving (Show,Read)

testError :: String -> TestStatus
testError = TestError
isTestError :: TestStatus -> Bool
isTestError (TestError _) = True
isTestError _ = False

testFailure :: String -> (Maybe FilePath) -> TestStatus
testFailure errMsg mReport = TestFailure errMsg mReport
testFailNoReport :: String -> TestStatus
testFailNoReport errMsg = testFailure errMsg Nothing
testFailWithReport :: String -> FilePath -> TestStatus
testFailWithReport errMsg report = testFailure errMsg (Just report)

testOk :: PerfMeasure -> Maybe FilePath -> TestStatus
testOk measure report = TestOk (Just measure) report
testOkUntimed :: Maybe FilePath -> TestStatus
testOkUntimed report = TestOk Nothing report
testOkNoReport :: PerfMeasure -> TestStatus
testOkNoReport m = testOk m Nothing
testOkWithReport :: PerfMeasure -> FilePath -> TestStatus
testOkWithReport m r = testOk m (Just r)

isTestOk :: TestStatus -> Bool
isTestOk (TestOk _ _) = True
isTestOk _ = False

formatInputSize :: (Real a) => Test -> a -> String
formatInputSize testinfo q = formatUnits q (preferredScale testinfo) (inputUnit testinfo)

instance Pretty TestResult where
  pretty (TestResult testinfo testargs teststatus) =
    pretty' ( text (testName testinfo) <+> hsep (map text testargs) ) teststatus
    where
    pretty' ctx (TestError errMsg) =
      ctx <+> text ("ERROR: "++errMsg)
    pretty' ctx (TestFailure errMsg report) =
      ctx <+> text ("FAILED: ")
      $+$ (nest 4 . vcat . catMaybes)
          [ Just (ppErrorMessage errMsg),
            fmap (ppFileRef "report") report ]
    pretty' ctx (TestOk measure report) =
      ctx <+> text "succeeded" <+> stats
      $+$ (nest 4 . vcat . catMaybes)
          [ fmap (ppFileRef "result") report ]
      where
        stats =
          case measure of
            Nothing    -> empty
            Just (PerfMeasure (inpsize,ttime)) | ttime == 0 -> empty
                                               | otherwise  ->
              parens$
                    text (formatInputSize testinfo inpsize ++ " in " ++ formatSeconds ttime ++ ", ")
                <+> text (formatUnitsPerTime (inpsize `per` ttime) (preferredScale testinfo) (inputUnit testinfo) (scaleSecs Unit))


ppErrorMessage :: String -> Doc
ppErrorMessage =  vcat . map text . filter (not . null) . lines

ppFileRef :: String -> String -> Doc
ppFileRef info file = text $ "See "++info++" file: `"++file++"'"

-- =============
-- = Test Runs =
-- =============

-- | Result of a parser test run
data TestRun =
    FatalError {
      fatalErrMsg :: String,
      runArgs    :: [String]
    }
   | InitFailure {
      initFailMsg :: String,
      runArgs    :: [String]
    }
  | TestResults {
      testObject :: String,
      testInputFiles :: [FilePath],
      testResults :: Map String TestResult
    }
  deriving (Show,Read)

hasTestResults :: TestRun -> Bool
hasTestResults (TestResults _ _ _) = True
hasTestResults _ = False

instance Pretty TestRun where
  pretty (FatalError { fatalErrMsg = msg, runArgs = args}) =
    text ("Test aborted with fatal error: "++msg) <+> brackets (text "$CC"<+>hsep (map text args))
  pretty (InitFailure { initFailMsg = msg, runArgs = args }) =
    text ("Test initialization failed: "++msg) <+> brackets (text "$CC"<+>hsep (map text args))
  pretty tr = vcat $ map pretty (Map.elems $ testResults tr)

initFailure :: String -> [String] -> TestRun
initFailure msg args =
  InitFailure { runArgs = args, initFailMsg = msg }

emptyTestResults :: String -> [FilePath] -> TestRun
emptyTestResults obj inpFs = TestResults { testObject = obj, testInputFiles = inpFs, testResults = Map.empty }

-- | Insert a test
insertTest :: TestResult -> TestRun -> TestRun
insertTest _ (InitFailure _ _) = error "insertTest: initialization failed"
insertTest result trun = trun { testResults = Map.insert (testName $ testInfo result) result (testResults trun) }


