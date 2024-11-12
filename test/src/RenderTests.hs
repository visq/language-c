{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RenderTests.hs (executable)
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- This module renders test results into HTML files, much like pugs smoke tests.
-- We use stylesheets to color cells in the result table.
--
-- If JQuery and its tablesorter plugin is available, it is used.
--
-- Resources used:
--   ../res/style.css
--   ../res/jquery-latest.js ../res/jquery.tablesorter.js [optional]
--
-- TOOD: Display performance in detailled view too (maybe only if differs significantly from the average performance)
-- TODO: Sort the tests. The tablesorter javascript doesn't play nice with the browser's back-button
-----------------------------------------------------------------------------
module Main (main) where

import Control.Monad
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith,ExitCode(..))
import System.FilePath
import Text.Printf
import Text.XHtml

import Language.C.Test.Framework

-- | read the dat file containing the test-results
readTestRuns :: FilePath -> IO [TestRun]
readTestRuns = fmap (map read . lines) . readFile

-- Summarize a set of test runs
data TestSetResult = TestSetResult
  {
    testSetName :: String,
    allOk         :: Int,
    someFailed    :: Int,
    initErrors    :: Int,
    fatalErrors   :: Int,
    testSummaries :: Map String TestSummary,
    testRuns      :: [TestRun]
  }
initTestSetResult :: String -> [TestRun] -> TestSetResult
initTestSetResult tsname tsruns = TestSetResult { testSetName = tsname,
                                                    allOk = 0, someFailed = 0,
                                                    initErrors = 0, fatalErrors = 0,
                                                    testSummaries = Map.empty, testRuns = tsruns }
executedTests :: TestSetResult -> Int
executedTests tsr = allOk tsr + someFailed tsr
totalTestRuns :: TestSetResult -> Int
totalTestRuns tsr = executedTests tsr + fatalErrors tsr + initErrors tsr

-- Summarizes one specific test in a test suite
data TestSummary = TestSummary
  {
    sTestInfo     :: Test,
    numOk         :: Int,
    numFailed     :: Int,
    totalEntities :: Double,
    totalTime     :: Time
  }
  deriving (Show,Read)

throughput :: TestSummary -> Double
throughput ts = totalEntities ts  `per` totalTime ts

initSummary :: Test -> TestSummary
initSummary t = TestSummary { sTestInfo = t, numOk = 0, numFailed = 0, totalEntities = 0, totalTime = 0 }

-- =====================
-- = Compute summaries =
-- =====================

computeSummary :: String -> [TestRun] -> TestSetResult
computeSummary tsname testruns =
  foldr updateSetSummary (initTestSetResult tsname testruns) testruns

updateSetSummary :: TestRun -> TestSetResult -> TestSetResult
updateSetSummary (FatalError _ _) s = s { fatalErrors = fatalErrors s + 1}
updateSetSummary (InitFailure _ _) s = s { initErrors = initErrors s + 1 }
updateSetSummary (TestResults _obj _files results) s =
  updateTestCount (Map.elems results) $
  s { testSummaries = foldr addToSummary (testSummaries s) (Map.elems results) }
  where
    updateTestCount rs  s' | all (isTestOk . testStatus) rs = s' { allOk = allOk s' + 1 }
                           | otherwise                      = s' { someFailed = someFailed s' + 1 }

addToSummary :: TestResult -> Map String TestSummary -> Map String TestSummary
addToSummary (TestResult testinfo _ teststatus) sums
  | isTestError teststatus = sums
  | otherwise = Map.alter alterSummary (testName testinfo) sums
  where
    alterSummary Nothing = alterSummary (Just (initSummary testinfo))
    alterSummary (Just s) = Just$
      case teststatus of
        (TestError _msg) -> s
        (TestFailure _msg _report) -> s { numFailed = succ (numFailed s) }
        (TestOk Nothing _report) ->
          s { numOk = succ (numOk s) }
        (TestOk (Just measure) _report) ->
          s { numOk = succ (numOk s),
              totalEntities = totalEntities s + realToFrac (processedEntities measure),
              totalTime = totalTime s + elapsedTime measure }

-- =========
-- = Files =
-- =========

datFile :: String -> FilePath
datFile testname = testname ++ ".dat"

indexFile :: String
indexFile = "index.html"

testSetFile :: TestSetResult -> String
testSetFile tss = testSetName tss ++ ".html"
-- ====================
-- = main entry point =
-- ====================

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    hPutStrLn stderr "Usage: ./RenderTests parser-version test-names"
    exitWith (ExitFailure 1)
  (parserVersion : _tests) <- getArgs
  let tests = map takeBaseName _tests
  testruns <- fmap (zip tests) $ mapM (readTestRuns.datFile) tests
  -- make file references relative to the current directory (for publishing)
  pwd <- getCurrentDirectory
  let normalizeFilePath = makeRelative pwd . normalise'

  -- compute summary
  let testresults = map (uncurry computeSummary) testruns
  -- export index file
  writeFile indexFile $
    htmlFile "Test result overviews" $
      indexContents parserVersion testresults
  -- export detailed file
  forM_ testresults $ \testResult ->
    writeFile (testSetFile testResult) $
      htmlFile ("Test results for "++ testSetName testResult) $
        detailedContents normalizeFilePath testResult

-- | create index.html
indexContents :: String -> [TestSetResult] -> Html
indexContents parserVersion tsresults =
       h1 << "Test results"
  +++  p  << ("Test with Language.C, "++parserVersion)
  +++  h2 << "Overview"
  +++  overviewTable tsresults ! [ identifier "overviewTable", theclass "tablesorter" ]
  +++  h2 << "Test Summaries"
  +++  concatHtml (map testSummary tsresults)
  where
    overviewTable results =
      mkTableWithSummaryRow
        ["test set name","total tests", "init error", "fatal error", "tests run", "all tests ok", "some tests failed" ]
        (map overviewRow results)
        (overviewSummaryRow results)
    overviewRow tsr = testSetLink tsr :
                      map (toHtml.show) [totalTestRuns tsr, initErrors tsr, fatalErrors tsr,
                                         executedTests tsr, allOk tsr, someFailed tsr ]
    overviewSummaryRow rs = stringToHtml "Total" :
                            map (toHtml.show)  [ sumMap totalTestRuns rs, sumMap initErrors rs, sumMap fatalErrors rs,
                                                 sumMap executedTests rs, sumMap allOk rs, sumMap someFailed rs]
    sumMap f = sum . map f
    testSetLink tsr = (anchor << testSetName tsr) ! [href (testSetFile tsr)]
    testSummary tsr =
          h3 << testSetLink tsr
      +++ summaryView tsr

-- | create testset.html
detailedContents :: (FilePath -> FilePath) -> TestSetResult -> Html
detailedContents normRef tsr =
      (anchor << "Contents") ! [href "index.html"]
  +++ h1 << ("Test Results for " ++ testSetName tsr)
  +++ h2 << "Summary"
  +++ summaryView tsr
  +++ h2 << "Detailed View"
  +++ detailedView normRef tsr

-- ==================
-- = HTML rendering =
-- ==================

mkTable :: (HTML hd) => [hd] -> [[Html]] -> Html
mkTable tableHeader tableRows =
  table $
        thead << (tr << map (th <<) tableHeader)
    +++ tbody << concatHtml (map (tr . concatHtml . map td) tableRows)

mkTableWithSummaryRow :: (HTML hd, HTML lst) => [hd] -> [[Html]] -> [lst] -> Html
mkTableWithSummaryRow tableHeader tableRows tableLast =
  table $
        thead << (tr << map (th <<) tableHeader)
    +++ tbody << concatHtml (map (tr . concatHtml . map td) tableRows)
    +++ tr (concatHtml $ map (\c -> (td << c) ! [ theclass "last_row" ]) tableLast)

tablesorterImport :: [String] -> Html
tablesorterImport tids =
    (script << "") ! [ thetype "text/javascript", src "../res/jquery-latest.js"]
    +++ (script << "") ! [ thetype "text/javascript", src "../res/jquery.tablesorter.js"]
    +++ concatHtml (map (addSort . ('#':)) tids)
    where
       addSort tid = (script <<  primHtml ("$(function() { $(" ++ quoteString tid ++  ").tablesorter({ widgets: ['zebra'] } ); });"))
                     ! [ thetype "text/javascript" ]
       quoteString s = ('"' : s) ++ "\""

htmlFile :: String -> Html -> String
htmlFile reportTitle thebody = prettyHtml $
  header <<
    (
          (thetitle << reportTitle)
      +++ (thelink << "") ! [ rel "stylesheet", href "../res/style.css", thetype "text/css" ]
      +++ tablesorterImport ["reportTable", "overviewTable"] -- hardcoded
    )
  +++ body thebody

-- =====================
-- = Rendering Summary =
-- =====================

-- * Summary of XXX.dat
-- Executed %d out of %d tests
-- Summary-Table
summaryView :: TestSetResult -> Html
summaryView tsr =
      p << (printf "Executed %d out of %d tests"  (length $ filter hasTestResults runs) (length runs) :: String)
  +++  summaryTable (Map.elems $ testSummaries tsr) ! [ identifier ("table_" ++ testSetName tsr) ]
  where
    runs = testRuns tsr


summaryTable :: [TestSummary] -> Html
summaryTable summaries = mkTable tblHeader (map mkRow summaries)
  where
    tblHeader = words "Test Ok Failed InputSize Time Throughput"
    mkRow  = summaryEntries
    summaryEntries ts =
      let testinfo = sTestInfo ts in
      map stringToHtml $
        [
          testName testinfo,
          show$ numOk ts,
          show$ numFailed ts
        ] ++ (if totalTime ts /= 0 then timeSummary testinfo ts else replicate 3 "n/a")
    timeSummary testinfo ts = [
        formatUnitsSafe (totalEntities ts) (preferredScale testinfo) (inputUnit testinfo),
        formatTimeSafe (totalTime ts) (scaleSecs Unit),
        formatUnitsPerSecond (throughput ts) (preferredScale testinfo) (inputUnit testinfo)
      ]

--
-- create HTML for detailled view
detailedView :: (FilePath -> FilePath) -> TestSetResult -> Html
detailedView normRef tsr =
       h1 (toHtml "Detailed Report")
  +++  detailedTable (Set.toList allKeys) (testRuns tsr) ! [ identifier "reportTable", theclass "tablesorter" ]
  where
    allKeys = Set.fromList . map (testName . sTestInfo) . Map.elems . testSummaries $ tsr

    detailedTable testkeys runs = table $
            (thead << detailedHeader ("Test Objective" : "Input Files" : testkeys))
        +++ (tbody << aboves (map (detailedRow testkeys) runs))
    detailedHeader testkeys = besides $ map (th <<) testkeys

    detailedRow _testkeys (FatalError msg args) = cell$ td fatalErr ! [ theclass "fatal_error" ]
      where
      fatalErr =      toHtml ("Fatal Error: "++show args)
                  +++ thediv (linesToHtml $ lines msg)  ! [ theclass "errmsg_box" ]

    detailedRow _testkeys (InitFailure msg args) = cell$ td initError ! [ theclass "init_error" ]
      where
      initError =     toHtml ("Fatal Initialization Error on " ++ show args)
                  +++ thediv (linesToHtml $ lines msg) ! [ theclass "errmsg_box" ]

    detailedRow testkeys (TestResults testobject filesUnderTest results) =
      cell (td << testobject)
      `beside` filesCell filesUnderTest
      `beside` besides (map (detailedCell results) testkeys)

    filesCell :: [FilePath] -> HtmlTable
    filesCell = cell . td . concatHtml . map fileref where
      fileref fp = (anchor << takeFileName fp) ! [href $ normRef fp] +++ br

    detailedCell :: Map.Map String TestResult -> String -> HtmlTable
    detailedCell results key =
      cell$ case Map.lookup key results of
        Nothing                                        -> td (toHtml "n/a") ! [ theclass "not_avail"]
        Just (TestResult _testinfo _testargs teststatus) -> statusCell teststatus

    statusCell  :: TestStatus -> Html
    statusCell (TestError errMsg)              = (td << errMsg) ! [ theclass "test_error"]
    statusCell (TestFailure errMsg reportfile) = (td << failureCell errMsg reportfile) ! [ theclass "test_fail"]
    statusCell (TestOk measure mResultFile) = (td << okCell measure mResultFile) ! [ theclass "test_ok"]

    failureCell :: String -> Maybe FilePath -> Html
    failureCell errMsg (Just report) = anchor (toHtml "Failure")  ! [href $ normRef report, title errMsg]
    failureCell errMsg Nothing = toHtml $ "Failure: "++errMsg

    okCell :: Maybe PerfMeasure -> Maybe FilePath -> Html
    okCell measure mReport = addRef mReport "Ok " +++ measureInfo measure ! [ theclass "time_info" ]
      where addRef Nothing info  = toHtml info
            addRef (Just f) info = (anchor << info) ! [href $ normRef f]
            measureInfo Nothing = noHtml
            measureInfo (Just m) = thespan << formatTimeAuto (elapsedTime m)

-- extended Filepath.normalise
-- we want to have @normalise' "/Foo/./bar/.././../baz"@ ==> @"/baz"@
-- Do not know how to accomplish this with System.FilePath ...
normalise' :: FilePath -> FilePath
normalise' = joinPath . reverse . foldl removeDotDot [] . splitPath . normalise
  where
    removeDotDot (dircomp:ds) dotDot | dropTrailingPathSeparator dotDot == "..", not (isAbsolute dircomp) = ds
    removeDotDot (dircomp:ds) dot    | dropTrailingPathSeparator dot == "." = dircomp:ds
    removeDotDot ds c = c:ds
