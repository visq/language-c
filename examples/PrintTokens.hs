-- Minimal example: parse a file, and pretty print it again
module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Text.PrettyPrint.HughesPJ

import Language.C              -- simple API
import Language.C.System.GCC   -- preprocessor used
import Language.C.System.Preprocess
import Language.C.Data
import Language.C.Data.Name
import Language.C.Parser (lexC, CToken)

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return

main :: IO ()
main = do
    let usageErr = (hPutStrLn stderr (usageMsg "./ParseAndPrint") >> exitWith (ExitFailure 1))
    args <- getArgs
    when (length args < 1) usageErr
    let (opts,input_file) = (init args, last args)

    -- preprocess
    input_stream <- if not (isPreprocessed input_file)
      then  let cpp_args = (rawCppArgs opts input_file) { cppTmpDir = Nothing }
            in  runPreprocessor (newGCC "gcc") cpp_args >>= handleCppError
      else  readInputStream input_file
    -- lex and collect tokens
    tokens <- errorOnLeft "lexer failed" $ execLexer input_stream (initPos input_file)
    -- print tokens
    mapM_ (\(ix,t) -> putStrLn ("Token " ++ show ix ++ ": " ++ show t)) $
      zip [1..] (reverse tokens)
    where
    handleCppError (Left exitCode) = fail $ "Preprocessor failed with " ++ show exitCode
    handleCppError (Right ok)      = return ok
    execLexer :: InputStream -> Position -> Either ParseError [CToken]
    execLexer input init_pos =
      case execParser lexP input init_pos [] (namesStartingFrom 0) of
        Left e       -> Left e
        Right (vs,_) -> Right vs
    lexP :: P [CToken]
    lexP = go []
      where
        go :: [CToken] -> P [CToken]
        go xs = lexC $ \tok -> case tok of
          CTokEof -> return xs
          _ -> go (tok:xs)

