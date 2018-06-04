{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances, ExistentialQuantification  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GenericTree
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  unspecified
--
-- This module can be used to debug AST trees, by converting them into a
-- generic tree with debug information.
-----------------------------------------------------------------------------
module GenericTree where
import Language.C
import Language.C.Syntax.AST
import Data.Tree
import Data.Typeable
import Data.Maybe
data CastT = CastT { castTo :: forall a. (Typeable a) => (a -> (Maybe a)) }
data AstNode =
      AstNode String (TypeRep, CastT) (Maybe NodeInfo)
    | InfoNode String
    | ListNode String
    | ConstNode CConst
    | IdentNode Ident

dynRep :: (Typeable a) => a -> (TypeRep, CastT)
dynRep a = (typeOf a, CastT (\_ -> cast a))

leaf :: AstNode -> Tree AstNode
leaf n = Node n []

constLeaf :: CConst -> Tree AstNode
constLeaf = leaf . ConstNode

identLeaf = leaf . IdentNode

infoLeaf :: String -> Tree AstNode
infoLeaf = leaf . InfoNode

renderLeaf :: (Typeable a, CNode a, Pretty v) => String -> a -> v -> Tree AstNode
renderLeaf lab t v = leaf $ AstNode (addLab.show.pretty $ v) (dynRep t) (Just $ nodeInfo t)
    where addLab s | null lab = s
                   | otherwise = lab ++ ": " ++ s

emptyList :: Tree AstNode
emptyList = Node (ListNode "<empty>") []

listNode :: (TreeView a) => String -> [a] -> Tree AstNode
listNode lab = Node (ListNode lab) . map (treeView (init lab))

attrsNode :: [CAttr] -> Tree AstNode
attrsNode = listNode "attributes"

node :: (Typeable t, CNode t) => String -> t -> [Tree AstNode] -> Tree AstNode
node lab v kids = Node (AstNode lab (dynRep v) (Just $ nodeInfo v)) (filter (not.isEmptyList) kids) where
    isEmptyList (Node (ListNode _) []) = True
    isEmptyList _ = False

instance Show AstNode where
    show (AstNode name _ _) = name
    show (ListNode str) = str
    show (ConstNode c) = (show.pretty) c
    show (IdentNode i) = show i
    show (InfoNode s) = s

class TreeView n where
    -- it is ok to implement only one of treeView or treeView'
    treeView :: String -> n -> Tree AstNode
    treeView' :: n -> Tree AstNode
    treeView' n = treeView (defaultLabel n) n
    defaultLabel :: n -> String
    defaultLabel _ = "ast-node"

instance TreeView (Tree AstNode) where
    treeView _ = id
instance TreeView Ident where
    treeView _ = leaf . IdentNode
    defaultLabel = identToString
instance TreeView CTranslUnit where
    treeView name t@(CTranslUnit decls ni) =
        node name t $ map treeView' decls
    defaultLabel (CTranslUnit _ ni) = maybe "<nofile>" id (fileOfNode ni)
instance TreeView CExtDecl where
    treeView _ (CDeclExt decl) = treeView "ext-decl" decl
    treeView _ (CFDefExt fundef) = treeView "ext-fundef" fundef
    treeView _ t@(CAsmExt asm ni) = node "ext-asm" t [constLeaf $ liftStrLit asm]
instance TreeView CFunDef where
    treeView lab t@(CFunDef specs declr params stat ni) =
        node (addName declr) t $
            [ listNode "fun-def-specs" specs,
              treeView "fun-def-declr" declr,
              listNode "old-style-param-decls" params,
              treeView "fun-def-stmt" stat ]
        where
        addName (CDeclr (Just ide) _ _ _ _) = lab ++ " " ++ show ide
        addName _ = lab
instance TreeView CDecl where
   treeView lab t@(CDecl specs decllist ni) = node (addNames lab) t $
        [ listNode "decl-specs" specs,
          listNode "declaration-body" (map declListEntry decllist)
        ]
    where
    declListEntry (Just declr,Nothing,Nothing) = treeView "declr" declr
    declListEntry (declr,initi,bitsize) = listNode "decl-entry" $
        catMaybes
        [ fmap (treeView "declr") declr,
          fmap (treeView "initializer") initi,
          fmap (treeView "bitsize") bitsize
        ]
    addNames lab = case catMaybes (map nameOfEntry decllist) of 
                    [] -> lab
                    names -> lab ++ " {" ++ unwords (map show names) ++ "}"
    nameOfEntry (Just (CDeclr (Just ide) _ _ _ _),_,_) = Just ide
    nameOfEntry _ = Nothing
instance TreeView CDeclr where
    treeView lab t@(CDeclr ide derived asm attrs ni) =
        node (addName lab ide) t $
            [ maybe emptyList identLeaf ide,
              listNode "type-derivations" derived,
              attrsNode attrs
            ]
        where 
        addName lab = maybe lab (\s -> lab ++ " " ++ show s) 

instance TreeView CDerivedDeclr where
    treeView lab t@(CPtrDeclr quals ni) = node "ptr-deriv" t $ map (treeView "ptr-qual") quals
    treeView lab t@(CArrDeclr quals sz ni) = node "arr-deriv" t $
        [ listNode "arr-quals" quals,
          treeView "arr-size" sz ]
    treeView lab t@(CFunDeclr params attrs ni) = node "fun-deriv" t (paramsView++attrsView)
        where
        paramsView =
            case params of
                Right (params,variadic) -> [listNode "params" params] ++ (if variadic then [infoLeaf "variadic"] else [])
                Left oldStyle -> [listNode "old-style-param-names" oldStyle]
        attrsView = [listNode "fun-attrs" attrs]

instance TreeView CArrSize where
    treeView _ (CNoArrSize True) = infoLeaf $ "variable size"
    treeView _ (CNoArrSize False) = emptyList
    treeView lab (CArrSize False c) = treeView lab c
    treeView lab (CArrSize True c) = Node (ListNode lab) [ infoLeaf "static size", treeView "size" c ]

instance TreeView CStat where
    treeView _ t =
        case t of
            CLabel ide stat attrs _ -> node "label" t [treeView "label-ident" ide,attrsNode attrs,treeView "label-stmt" stat]
            CCase e s _ -> node "case" t [treeView "case" e, treeView "case-body" s]
            CCases el eu s _ -> node "cases" t [treeView "case-lower" el, treeView "case-upper" eu, treeView "case-body" s]
            CCompound i bis _ -> node "block" t [listNode "idents" i,listNode "block-items" bis]
            CDefault s _ -> node "case-default" t [treeView "default-body" s]
            CExpr Nothing _ ->  node "empty-stmt" t []
            CExpr (Just e) _ -> node "expr-stmt" t [ treeView "expr" e ]
            CIf ife thens elses _ -> node "if-stmt" t $ [ treeView "if" ife, treeView "then-stmt" thens ] ++
                                                        (maybe [] return $ fmap (treeView "else-stmt") elses)
            CSwitch es s _ -> node "switch-stmt" t $ [treeView "switch-body" s]
            CWhile e s doWhile _ -> node (if doWhile then "do-while" else "while") t $
                                        [treeView "while-guard" e, treeView "while-body" s]
            CFor eInit eGuard eUpd stat _ -> node "for" t $ [ maybe (infoLeaf "no-for-init") id (initTree eInit),
                                                              maybe (infoLeaf "no-for-guard") (treeView "for-guard") eGuard,
                                                              maybe (infoLeaf "no-for-update") (treeView "for-update") eUpd]
            CGoto ide _ -> node "goto" t [treeView "goto-label" ide]
            CGotoPtr expr _ -> node "goto-ptr" t [treeView "goto" expr]
            CCont _ -> node "continue" t []
            CBreak _ -> node "break" t []
            CReturn e _ -> node "return" t $ maybe [] return $ fmap (treeView "return") e
            CAsm asm _ -> node "asm-stmt" t [treeView "asm" asm]
     where
     initTree (Left eInit) = fmap (treeView "for-init") eInit
     initTree (Right initStmt) = Just (treeView "for-c99-init" initStmt)

instance TreeView CBlockItem where
    treeView lab (CBlockStmt stmt) = treeView "block-stmt" stmt
    treeView lab (CBlockDecl decl) = treeView "block-decl" decl
    treeView lab (CNestedFunDef fundef) = treeView "nested-fun-def" fundef

instance TreeView CDeclSpec where
    treeView _ t@(CStorageSpec v) = renderLeaf "" t v
    treeView _ t@(CTypeSpec v)    = renderLeaf "" t v
    treeView _ t@(CTypeQual v)    = renderLeaf "" t v
addLab "" = id
addLab s  = ((s++).(" "++))
instance TreeView CExpr where
    treeView mlab t = 
        let lab = case mlab of "" -> (\s -> s ++ "-expr"); l -> (\s -> l ++ " " ++ s ++ "-expr") in
        case t of
          CComma es _ -> node (lab "comma") t $ map (treeView "expr") es
          CAssign op e1 e2 _ -> node (lab "assign-expr") t [ treeView "assign-op" op, treeView "lhs" e1, treeView "rhs" e2]
          CCond guardE mTrueE falseE _ -> node (lab "cond") t [ treeView "guard" guardE, 
                                                               maybe (infoLeaf "ommited-true") (treeView "true") mTrueE,
                                                               treeView "false" falseE]
          CBinary op e1 e2 _ -> node (lab "bin") t [treeView "binary-op" op, treeView "left" e1, treeView "right" e2 ]
          CCast decl expr _ -> node (lab "cast") t [ treeView "cast-type" decl, treeView "cast" expr]
          CUnary unop expr _ -> node (lab "") t [treeView "unary-op" unop, treeView "sub" expr]
          CSizeofExpr expr _ -> node (lab "sizeof") t [ treeView "expr" expr ]
          CSizeofType decl _ -> node (lab "sizeof") t [ treeView "type-decl" decl ]
          CAlignofExpr expr _ -> node (lab "alignof") t [ treeView "expr" expr]
          CAlignofType decl _ -> node (lab "alignof-type") t [ treeView "type-decl" decl]
          CComplexReal expr _ -> node (lab "complex-real") t [ treeView "expr" expr]
          CComplexImag expr _ -> node (lab "complex-imag") t [ treeView "expr" expr]
          CIndex target ix _ -> node (lab "index") t [ treeView "target" target, treeView "index" ix]
          CCall fun args _ -> node (lab "call") t [ treeView "callee" fun, listNode "call-args" args ]
          CMember expr member False _ -> node "member-of-struct" t [ treeView "struct" expr, treeView "member" member]
          CMember expr member True _ -> node "member-of-ptr" t [ treeView "ptr" expr, treeView "member" member]
          CVar ide _ -> node (lab "var") t [ identLeaf ide ]
          CConst c -> treeView "const" c
          CCompoundLit decl initList _ -> node (lab "compound-literal") t [ treeView "compound-type" decl, 
                                                                      initListTree "compound-lit" initList ]
          CStatExpr stat _ -> node (lab "stmt") t [ treeView "expr-stmt" stat ]
          CLabAddrExpr label _ -> node (lab "address-of-label") t [ treeView "label" label ]
          CBuiltinExpr builtin -> treeView "builtin" builtin

instance TreeView CBuiltin where
    treeView lab t@(CBuiltinVaArg expr decl _) = node "builtin-va-arg" t [ treeView "arg-ptr" expr, treeView "type" decl ] 
    treeView lab t@(CBuiltinOffsetOf typ desigs _) = node "builtin-offset-of" t [ treeView "type" typ, listNode "designators" desigs ]
    treeView lab t@(CBuiltinTypesCompatible ty1 ty2 _) = node "builtin-types-compatible" t [ treeView "type-1" ty1, treeView "type-2" ty2]

instance TreeView CDesignator where
    treeView lab t = renderLeaf lab t t
instance TreeView CAttr where
    treeView lab t = renderLeaf lab t t
instance TreeView CTypeQual where
    treeView lab t = renderLeaf "" t t
instance TreeView CInit where
    treeView _ (CInitExpr e _) = treeView "init" e
    treeView lab t@(CInitList l _) = renderLeaf lab t t
initListTree :: String -> CInitList -> Tree AstNode
initListTree lab = listNode lab . map initComp where
    initComp (desigs,init) = listNode "member-init" [ listNode "designators" desigs, treeView "sub-init" init ]
instance TreeView CAsmStmt where
    treeView lab t = renderLeaf lab t t
instance TreeView CAssignOp where
    treeView _ t = infoLeaf $ (show.pretty) t
instance TreeView CBinaryOp where
    treeView _ t = infoLeaf $ (show.pretty) t
instance TreeView CUnaryOp where
    treeView _ t = infoLeaf $ (show.pretty) t
instance TreeView CConst where
    treeView lab t = renderLeaf lab t t
    
-- data CInit
-- type CInitList = [([CDesignator], CInit)]
-- data CDesignator
-- = CArrDesig CExpr NodeInfo
-- | CMemberDesig Ident NodeInfo
-- | CRangeDesig CExpr CExpr NodeInfo
-- data CAsmStmt = CAsmStmt (Maybe CTypeQual) CStrLit [CAsmOperand] [CAsmOperand] [CStrLit] NodeInfo
-- data CAsmOperand = CAsmOperand (Maybe Ident) CStrLit CExpr NodeInfo
-- data CConst
-- = CIntConst CInteger NodeInfo
-- | CCharConst CChar NodeInfo
-- | CFloatConst CFloat NodeInfo
-- | CStrConst CString NodeInfo
-- data CStrLit = CStrLit CString NodeInfo
--
-- data CStructUnion = CStruct CStructTag (Maybe Ident) (Maybe [CDecl]) [CAttr] NodeInfo
-- data CStructTag
-- = CStructTag
-- | CUnionTag
-- data CEnum = CEnum (Maybe Ident) (Maybe [(Ident, Maybe CExpr)]) [CAttr] NodeInfo
-- partitionDeclSpecs :: [CDeclSpec] -> ([CStorageSpec], [CAttr], [CTypeQual], [CTypeSpec], Bool)
-- data CStorageSpec
-- = CAuto NodeInfo
-- | CRegister NodeInfo
-- | CStatic NodeInfo
-- | CExtern NodeInfo
-- | CTypedef NodeInfo
-- | CThread NodeInfo
-- data CTypeSpec
-- = CVoidType NodeInfo
-- | CCharType NodeInfo
-- | CShortType NodeInfo
-- | CIntType NodeInfo
-- | CLongType NodeInfo
-- | CFloatType NodeInfo
-- | CDoubleType NodeInfo
-- | CSignedType NodeInfo
-- | CUnsigType NodeInfo
-- | CBoolType NodeInfo
-- | CComplexType NodeInfo
-- | CSUType CStructUnion NodeInfo
-- | CEnumType CEnum NodeInfo
-- | CTypeDef Ident NodeInfo
-- | CTypeOfExpr CExpr NodeInfo
-- | CTypeOfType CDecl NodeInfo
-- isSUEDef :: CTypeSpec -> Bool
-- data CTypeQual
-- = CConstQual NodeInfo
-- | CVolatQual NodeInfo
-- | CRestrQual NodeInfo
-- | CInlineQual NodeInfo
-- | CAttrQual CAttr
-- data CAttr = CAttr Ident [CExpr] NodeInfo
