{-# LANGUAGE PatternGuards #-}
-- /ComputeSize 'comp' compute_size.c | gcc -x c -o compute_size_hs - && ./compute_size_hs
module Main where
import System.Environment ; import System.IO
import System.FilePath    ;
import Control.Arrow      ; import Control.Monad
import Data.Map (Map)     ; import qualified Data.Map as Map
import Data.Maybe         ; import Data.Function (fix)
import Data.Generics      ; import Data.List

import Language.C   -- Language.C.{Data,Syntax,Pretty,Parser,InputStream}
import Language.C.Analysis        -- analysis API
import Language.C.Analysis.TypeUtils -- analysis type utilities
import Language.C.System.GCC      -- preprocessor used
import Language.C.Analysis.Export -- [starting point for exporting SemRep to AST]

main :: IO ()
main = do
    let usage = error "Example Usage: ./ComputeSize 'pattern' -I/usr/include my_file.c"
    args <- getArgs
    when (length args < 2) usage
    let (pat,args')   = (head &&& tail) args
    let (opts,c_file) = (init &&& last) args'

    let compiler = newGCC "gcc"
    ast <- parseCFile compiler Nothing opts c_file >>= checkResult "[parsing]"

    (globals,warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
    mapM_ (hPutStrLn stderr . show) warnings
    putStrLn "#include <stdio.h>"
    print $ pretty (generateSizeTests pat globals)
    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

generateSizeTests :: String -> GlobalDecls -> CTranslUnit
generateSizeTests pat globals =
      flip CTranslUnit undefNode $
      -- forward declare all composite types
         map declareComp (Map.elems all_comps)
      -- declare enums
      ++ map defineEnum (Map.elems all_enums)
      -- define all neccessary composite type
      ++ map defineComp referenced_comps
      -- define typeDefs
      ++ (map defineTyDef (Map.elems reverse_typeDefs))
      -- generate size tests for named comps
      ++ [ genSizeTest reverse_typeDefs (Map.elems comps_of_interest) ]
    where
    (all_enums,all_comps) = Map.mapEither splitEnums (gTags globals)
    comps = Map.mapMaybe fromComp (gTags globals)
    comps_of_interest  = filterDefs pat comps
    referenced_comps   = computeRefClosure comps comps_of_interest
    reverse_typeDefs   = Map.fromList . mapMaybe fromCompTyDef . Map.elems . filterDefs pat $ gTypeDefs globals
    splitEnums (CompDef su) = Right su
    splitEnums (EnumDef e) = Left e
    fromComp (CompDef struct_union) = Just struct_union
    fromComp (EnumDef _) = Nothing
    fromCompTyDef (TypeDef name ty _ _) =
      case ty of
        (DirectType (TyComp ref@(CompTypeRef sueref _tag _)) _ _) ->
            Just (sueref,(ref,name))
        _ -> Nothing

filterDefs :: (CNode v) => String -> Map k v -> Map k v
filterDefs pat = Map.filter isInCFile
    where
    isInCFile = maybe False ((pat `isPrefixOf`) . takeBaseName) . fileOfNode

-- a small fixpoint algorithm to find the correct order and all references
computeRefClosure :: Map SUERef CompType -> Map SUERef CompType -> [CompType]
computeRefClosure all_comps initial_comps =
    fixCont addReferenced ([], Map.elems initial_comps, (Map.empty,Map.empty))
    where
    fixCont f = fix $ \close args ->
        let args'@(result',todo',_) = f args in (if null todo' then reverse result' else close args')
    addReferenced (result,[],ms) = (result,[],ms)
    addReferenced (result,(t:ts),(visit,enter)) | Map.member (sueRef t) enter = (result,ts,(visit,enter))
                                                | Map.member (sueRef t) visit =
                                                (t:result,ts,(visit,Map.insert (sueRef t) t enter))
                                                | otherwise =
        let refd = referenced t in (result, refd++(t:ts), (Map.insert (sueRef t) t visit,enter))
    referenced (CompType _ _ members _ _) = mapMaybe getRefdComp members
    getRefdComp memberDecl = fromDirectRefdType (declType memberDecl) >>= fromCompTy
    fromCompTy (TyComp (CompTypeRef ref _ _))
        | (Just r) <- Map.lookup ref all_comps = Just r
        | otherwise = error $ "Internal Error: Could not find definition for "++show ref
    fromCompTy _ = Nothing
    fromDirectRefdType (DirectType tyname _ _) = Just tyname
    fromDirectRefdType (TypeDefType (TypeDefRef _ ref _) _ _) = (fromDirectRefdType) ref
    fromDirectRefdType (ArrayType ty _ _ _) = fromDirectRefdType ty
    fromDirectRefdType _ = Nothing

defineEnum :: EnumType -> CExtDecl
defineEnum ty = CDeclExt (CDecl (map CTypeSpec (exportEnumType $ ty)) [] undefNode)

declareComp :: CompType -> CExtDecl
declareComp ty = CDeclExt (CDecl (map CTypeSpec (exportCompTypeRef ty)) [] undefNode)

defineComp :: CompType -> CExtDecl
defineComp ty = CDeclExt (CDecl (map CTypeSpec (exportCompType $ derefTypeDefs ty)) [] undefNode)
    where
    derefTypeDefs ty' = everywhere (mkT derefTypeDef `extT` replaceEnum) ty'
    replaceEnum (TyEnum _) = TyIntegral TyInt
    replaceEnum dty = dty
    
defineTyDef :: (CompTypeRef, Ident) -> CExtDecl
defineTyDef (ctr,tydef) = CDeclExt (CDecl specs [CDeclI $ CDeclr (Just tydef) [] Nothing [] undefNode] undefNode)
  where 
  specs = [CStorageSpec (CTypedef undefNode)] ++ map CTypeSpec (exportCompTypeDecl ctr)

-- This is were we'd like to have quasi-quoting.
-- For now, as we lack any code generation facilies, we'll parse a string :)
genSizeTest :: Map SUERef (CompTypeRef,Ident) -> [CompType] -> CExtDecl
genSizeTest typeDefs tys =
      either (\e -> error $ "Failed to parse " ++ test ++ ": " ++ show e)
             fromExtDecl
             (parseC (inputStreamFromString test) (initPos "genSizeTest"))
    where
    fromExtDecl (CTranslUnit [decl] _ ) = decl
    fromExtDecl (CTranslUnit decls _) = error $ "Expected one declaration, but found: "++show (length decls)
    test = "int main() {" ++ concatMap checkSize tys ++ "}"
    checkSize (CompType sue_ref tag _ _ _) =
      case getTagStr sue_ref tag of
        Nothing  -> ""
        Just tag_str -> "printf(\""++ tag_str ++": %lu\\n\",sizeof(" ++ tag_str ++ ")); ";
    getTagStr ref@(AnonymousRef _) _tag =
      case Map.lookup ref typeDefs of
        Just (_,tyident) -> Just (identToString tyident)
        Nothing          -> Nothing -- ignoring inaccessible anonymous type
    getTagStr (NamedRef name) tag =
      Just (show tag ++ " " ++ identToString name)
