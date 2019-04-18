{-# LANGUAGE QuasiQuotes #-} 
module Main where
import Control.Monad
import System.Environment
import Language.C
import Language.C.System.GCC
import Text.Printf
import Text.PrettyPrint  as PP
import Text.PrettyPrint.HughesPJ
--import Here (here) 
main = do
    -- this is not the prettiest, but easiest solution
    let depth = 2
    putStrLn "#include <stdio.h>"
    print $ pretty $ parseCExtDecl $ show $
      text "int main(int argc, char**argv)" $+$
      (braces $
        stat_embed depth (stat1 depth) $+$
        stat_embed depth (stat2 depth) $+$
        text "return(0);")

parseCStat :: String -> CStat
parseCStat s = either (error.show) id $ execParser_ statementP (inputStreamFromString s) (initPos "<stdin>")
parseCExtDecl :: String -> CExtDecl
parseCExtDecl s = either (error.show) id $ execParser_ extDeclP (inputStreamFromString s) (initPos "<stdin>")

stat_embed :: Int -> CStat -> Doc
stat_embed k stat = braces $ nest 2 $
    decls $+$
    text "int r = 0;" $+$
    iteropen $+$
    (nest 2 stmt) $+$
    (nest 2 $ text "printf(\"%d\\n\",r);") $+$
    iterclose
 where
    stmt =  pretty stat
    decls = vcat $ map (\n -> text "int" <+> text(guardName n) PP.<> semi) [1..k]
    iteropen = vcat $ map (\n -> let gn = guardName n in text (printf "for(%s=0;%s<=1;%s++){" gn gn gn)) [1..k]
    iterclose = vcat $ replicate k (char '}')

guardName n = "g_"++show n
setR :: Int -> CStat
setR k = parseCStat $ printf "r = %d;" k
stat1 :: Int -> CStatement NodeInfo
stat1 depth = go depth
  where
    go depth = 
        case depth of
            n | n <= 1     -> CIf (guard n) (setR 1) (Just$ setR 2) u
              | otherwise  -> CIf (guard n) (go (n-1)) Nothing u
    cexpr = CExpr . Just
    vexpr s = CVar (internalIdent s) u
    guard n = vexpr (guardName n)
    u = undefNode
stat2 :: Int -> CStatement NodeInfo
stat2 depth =  CIf (guard depth) (go (depth-1)) (Just$ setR 2) u
  where
    go n | n == 0     = setR 1
         | otherwise  = CIf (guard n) (go (n-1)) Nothing u
    cexpr = CExpr . Just
    vexpr s = CVar (internalIdent s) u
    guard n = vexpr (guardName n)
    u = undefNode

