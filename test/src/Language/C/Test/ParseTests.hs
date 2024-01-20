{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ParseTests
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable
--
-- Provides a set of tests for the parser and pretty printer.
-----------------------------------------------------------------------------
module Language.C.Test.ParseTests (
-- * Misc helpers
time, lineCount, withFileExt,
-- * preprocessing
runCPP,
-- * Tests
parseTestTemplate, runParseTest,
ppTestTemplate, runPrettyPrint,
equivTestTemplate,runEquivTest,
compileTestTemplate, runCompileTest,
) where
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.List

import System.Directory
import System.Exit
import System.FilePath (takeBaseName, takeExtension)
import System.IO
import System.Process

import Language.C

import Language.C.Test.Environment
import Language.C.Test.Framework
import Language.C.Test.GenericAST
import Language.C.Test.TestMonad

-- ===================
-- = Misc            =
-- ===================

lineCount :: FilePath -> IO Int
lineCount = liftM (length . lines) . readFile

-- | change filename extension
withFileExt :: FilePath -> String -> FilePath
withFileExt filename ext = (stripExt filename) ++ "." ++ ext where
  stripExt fn =
    let basefn = takeBaseName fn in
    case (dropWhile (/= '.') . reverse) basefn of
      ('.' : s : ss) -> reverse (s : ss)
      _ -> basefn

-- =======
-- = CPP =
-- =======

-- | @(copiedFile,preprocessedFile) = runTestCPP origFile cppArgs@ copies the original
--   file to @copiedFile@ and then preprocesses this file using @gcc -E -o preprocessedFile cppArgs@.
runCPP :: FilePath -> [String] -> TestMonad (FilePath,FilePath)
runCPP origFile cppArgs = do
  -- copy original file (for reporting)
  cFile <- withTempFile_ (takeExtension origFile) $ \_ -> return ()
  copySuccess <- liftIOCatched (copyFile origFile cFile)
  case copySuccess of
    Left err -> errorOnInit cppArgs $ "Copy failed: " ++ show err
    Right () -> dbgMsg      $ "Copy: " ++ origFile ++ " ==> " ++ cFile ++ "\n"

  -- preprocess C file, if it isn't preprocessed already
  preFile <- case isPreprocessedFile cFile of
    False -> do
      dbgMsg $ "Preprocessing " ++ origFile ++ "\n"
      preFile     <- withTempFile_ ".i" $ \_hnd -> return ()
      (preErrReport, gccExitcode) <- withTempFile ".cpp_report" $ \errReportHnd -> do
         liftIO $ runProcess "gcc"  (["-E", "-o", preFile] ++ cppArgs ++ [origFile])
                  Nothing Nothing
                  Nothing Nothing (Just errReportHnd)
                  >>= waitForProcess
      case gccExitcode of
        ExitSuccess       ->  do
          modify $ addTmpFile preFile
          return preFile
        ExitFailure fCode -> do
          modify $ addTmpFile preErrReport
          errReport <- liftIO $ readFile preErrReport
          errorOnInit cppArgs $ "C preprocessor failed: " ++ "`gcc -E -o " ++ preFile ++ " " ++ (unwords cppArgs) ++ " " ++ origFile ++
                                "' returned exit code `" ++ show fCode ++ "'\n" ++
                                "Output from stderr: " ++ errReport
    True -> return cFile
  return (cFile,preFile)

-- ===============
-- = Parse tests =
-- ===============

parseTestTemplate :: Test
parseTestTemplate = Test
  {
    testName = "parse",
    testDescr = "parse the given preprocessed c file",
    preferredScale = Kilo,
    inputUnit = linesOfCode
  }

runParseTest :: FilePath           -- ^ preprocesed file
             -> Position           -- ^ initial position
             -> TestMonad (Either (String,FilePath) (CTranslUnit,PerfMeasure)) -- ^ either (errMsg,reportFile) (ast,(locs,elapsedTime))
runParseTest preFile initialPos = do
  -- parse
  dbgMsg $ "Starting Parse of " ++ preFile ++ "\n"
  ((parse,input),elapsed) <-
    time $ do input <- liftIO$ readInputStream preFile
              parse <- parseEval input initialPos
              return (parse,input)

  -- check error and add test
  dbgMsg $ "Parse result : " ++ eitherStatus parse ++ "\n"
  case parse of
    Left err@(ParseError (errMsgs, pos)) -> do
      report <- reportParseError err (inputStreamToString input)
      return $ Left $ (unlines (("Parse error in " ++ show pos) : errMsgs), report)
    Right header ->
      return $ Right $ (header,PerfMeasure (fromIntegral $ countLines input,elapsed))

reportParseError :: ParseError -> String -> TestMonad FilePath
reportParseError (ParseError (errMsgs,pos)) input = do
  withTempFile_ ".report" $ \hnd -> liftIO $ do
    pwd        <- getCurrentDirectory
    contextMsg <- getContextInfo pos
    hPutStr hnd $ "Failed to parse " ++ (posFile pos)
               ++ "\nwith message:\n" ++ concat errMsgs ++ " " ++ show pos
               ++ "\n" ++ contextMsg
               ++ "\nWorking dir: " ++ pwd
               ++ "\nPreprocessed input follows:\n\n" ++ input

-- ======================
-- = Pretty print tests =
-- ======================
ppTestTemplate :: Test
ppTestTemplate = Test
  {
    testName = "pretty-print",
    testDescr = "pretty-print the given AST",
    preferredScale = Kilo,
    inputUnit = linesOfCode
  }

runPrettyPrint :: CTranslUnit -> TestMonad ((FilePath, FilePath), PerfMeasure)
runPrettyPrint ast = do
    -- pretty print
    dbgMsg "Pretty Print ..."
    (fullExport,t) <-
      time $
        withTempFile_ "pp.c" $ \hnd ->
          liftIO $ hPutStrLn hnd $ show (pretty ast)
    modify $ addTmpFile fullExport
    locs <- liftIO $ lineCount fullExport

    dbgMsg $ " to " ++ fullExport ++ " (" ++ show locs ++ " lines)"++ "\n"

    -- export the parsed file, with headers via include
    dbgMsg $ "Pretty Print [report] ... "
    smallExport <- withTempFile_ "ppr.c" $ \hnd ->
      liftIO $ hPutStrLn hnd $ show (prettyUsingInclude ast)
    dbgMsg $ "to " ++ smallExport ++ "\n"

    lc <- liftIO $ lineCount fullExport
    return ((fullExport,smallExport), PerfMeasure (fromIntegral lc,t))

-- ===============
-- = Equiv Tests =
-- ===============
equivTestTemplate :: Test
equivTestTemplate = Test
  {
    testName = "equivalence check",
    testDescr = "check if two ASTs are equivalent",
    preferredScale = Unit,
    inputUnit = topLevelDeclarations
  }

runEquivTest :: CTranslUnit -> CTranslUnit -> TestMonad (Either (String, Maybe FilePath) PerfMeasure)
runEquivTest (CTranslUnit decls1 _) (CTranslUnit decls2 _) = do
  dbgMsg $ "Check AST equivalence\n"

  -- get generic asts
  (result,t) <- time $ do
    let ast1 = map (toGenericAST . normalizeAST) decls1
    let ast2 = map (toGenericAST . normalizeAST) decls2
    if (length ast1 /= length ast2)
      then
        return $ Left ("Length mismatch: " ++ show (length ast1) ++ " vs. " ++ show (length ast2), Nothing)
      else
        case find (\(_, (d1,d2)) -> d1 /= d2) (zip [0..] (zip ast1 ast2)) of
          Just (ix, (decl1,decl2)) -> do
            declf1 <- withTempFile_ ".1.ast"    $ \hnd -> liftIO $ hPutStrLn hnd (show $ pretty decl1)
            declf2 <- withTempFile_ ".2.ast"    $ \hnd -> liftIO $ hPutStrLn hnd (show $ pretty decl2)
            modify $ (addTmpFile declf1 . addTmpFile declf2)
            diff   <- withTempFile_ ".ast_diff" $ \_hnd -> return ()
            decl1Src <- liftIO $ getDeclSrc decls1 ix
            decl2Src <- liftIO $ getDeclSrc decls2 ix
            liftIO $ do
              appendFile diff ("Original declaration: \n" ++ decl1Src ++ "\n")
              appendFile diff ("Pretty printed declaration: \n" ++ decl2Src ++ "\n")
              _ <- system $ "diff -u '" ++ declf1 ++ "' '" ++ declf2 ++ "' >> '" ++ diff ++ "'" -- TODO: escape ' in filenames
              return ()
            return $ Left ("Declarations do not match: ", Just diff)
          Nothing -> return $ Right (length ast1)
  return $ either Left (\decls -> Right $ PerfMeasure (fromIntegral decls, t)) result

getDeclSrc :: [CExtDecl] -> Int -> IO String
getDeclSrc decls ix = case drop ix decls of
  [] -> error "getDeclSrc : Bad ix"
  [decl] -> readFilePos (posOf decl) Nothing
  (decl:(declNext:_)) | fileOf decl /= fileOf declNext -> readFilePos (posOf decl) Nothing
                       | otherwise -> readFilePos (posOf decl) (Just (posRow $ posOf declNext))
  where
    fileOf = posFile . posOf
    readFilePos pos mLineNext = do
      let lnStart = posRow pos - 1
      lns <- liftM (drop lnStart . lines) $ readFile (posFile pos)
      (return.unlines) $
        case mLineNext of
          Nothing -> lns
          (Just lineNext) -> take (lineNext - lnStart - 1) lns

-- ================
-- = Compile test =
-- ================
compileTestTemplate :: Test
compileTestTemplate = Test
  {
    testName = "compile",
    testDescr = "compile the given file using gcc",
    preferredScale = Kilo,
    inputUnit = linesOfCode
  }

runCompileTest :: [String] -> FilePath -> TestMonad (Either (String, FilePath) (FilePath, PerfMeasure))
runCompileTest args file = do
    -- compile
    dbgMsg "Compile ..."
    outFile   <- withTempFile_ ".o" $ \_hnd -> return ()
    (gccErrReport,(exitCode,t)) <- withTempFile ".cc_report" $ \errReportHnd -> do
        time $ liftIO $ runProcess "gcc"  (["-o", outFile,"-std=gnu99"] ++ args ++ [file])
                                Nothing Nothing
                                Nothing Nothing (Just errReportHnd)
                         >>= waitForProcess
    case exitCode of
        ExitSuccess ->  do
            liftIO $ removeFile gccErrReport
            locs <- liftIO $ lineCount file
            return . Right $ (outFile, PerfMeasure (fromIntegral locs, t))
        ExitFailure fCode -> do
            return . Left $ ("gcc "++concat (intersperse " " args)++" "++file++ " failed with exit code "++show fCode,
                             gccErrReport)

-- ===========
-- = Helpers =
-- ===========

--  make sure parse is evaluated
-- Rational: If we no wheter the parse result is an error or ok, we already have performed the parse
parseEval :: InputStream -> Position -> TestMonad (Either ParseError CTranslUnit)
parseEval input initialPos =
  case parseC input initialPos of
    Left  err -> return $ Left err
    Right ok ->  return $ Right ok

eitherStatus :: Either a b -> String
eitherStatus = either (const "ERROR") (const "ok")

getContextInfo :: Position -> IO String
getContextInfo pos = do
  content <- readFile (posFile pos) `catch` handleIOError
  return $
    case splitAt (posRow pos - 1) (lines content) of
      ([],[]) -> "/* No Input */"
      ([],ctxLine : post) -> showContext [] ctxLine (take 1 post)
      (pre,[]) -> showContext [last pre] "/* End Of File */" []
      (pre,ctxLine : post) -> showContext [last pre] ctxLine (take 1 post)
  where
    handleIOError :: IOException -> IO String
    handleIOError _ = return ""
    showContext preCtx ctx postCtx = unlines $ preCtx ++ [ctx, replicate (posColumn pos - 1) ' ' ++ "^^^"] ++ postCtx

