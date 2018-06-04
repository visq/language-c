{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
-- Example demonstrating how link the AST back to the source code,
-- using a simple heuristic
module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad ()
import Control.Monad.Error as Err
import Data.List
import Text.PrettyPrint.HughesPJ
import Data.Tree
import Data.Maybe (fromMaybe)
--import Debug.Trace


import Language.C              -- simple API
import Language.C.Data.Node
import GenericTree
import SourceBrowser

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["input_file.i"])
errorOnLeftM :: (MonadError e m, Err.Error e, Show a) => String -> m (Either a b) -> m b
errorOnLeftM msg action = either (throwError . strMsg . showWith) return =<< action
    where showWith s = msg ++ ": " ++ (show s)

main :: IO ()
main = do
    let usageErr = (hPutStrLn stderr (usageMsg "./Annotate") >> exitWith (ExitFailure 1))
    -- get command line arguments
    args <- getArgs
    c_file <- case args of
                [a1] -> return a1
                _    -> usageErr
    -- parse the file
    ast <- errorOnLeftM "Parse Error" (parseCFilePre c_file)
    -- split the AST by input file
    let groups = groupAstBySourceFile ast
    -- show the generic tree
    putStrLn . drawTree . fmap show . (uncurry treeView) $ last groups
    -- run the source view
    runGTK (map (uncurry treeView) groups) c_file

groupAstBySourceFile :: CTranslUnit -> [(FilePath,CTranslUnit)]
groupAstBySourceFile (CTranslUnit decls _) = 
    map (\decls -> (fileOfNode' (head decls), CTranslUnit decls (topNodePos decls))) .
    groupBy (\a b -> (fileOfNode' a) == fileOfNode' b) $ decls
    where
    fileOfNode' = maybe "<no-file>" id  . fileOfNode
    topNodePos decls = 
        let lastDecl = nodeInfo (last decls) in
        mkNodeInfoPosLen (posOf (head decls)) (getLastTokenPos lastDecl)
