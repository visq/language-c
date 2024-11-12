{-# LANGUAGE DeriveDataTypeable #-}

module DeriveTest where
import Language.C.Data.Node
import Language.C.Data.Position
import Data.Data
data ExplicitNodeInfo = ExplicitNodeInfo1 NodeInfo Int
                      | ExplicitNodeInfo2 Int NodeInfo
                      | ExplicitNodeInfo3 Int NodeInfo Int
                   deriving (Data {-! ,CNode !-})
data OneArgNodeInfo = ExplicitNodeInfo4 Int NodeInfo
                    | Delegator ExplicitNodeInfo
                   deriving (Data {-! ,CNode !-})
data PolyVarNodeInfo a = PolyCon2 Int Int a
                       | PolyCon1 Int a
                       | PolyCon0 a
                       | PolyDelegator OneArgNodeInfo
                   deriving (Data {-! ,CNode !-})

-- -- Should fail
-- data PolyVarNodeInfo a b = PolyCon2 Int Int a
--                         | PolyCon1 a b
--                   deriving (Data {-! CNode !-})
--


-- GENERATED START

instance CNode ExplicitNodeInfo where
        nodeInfo (ExplicitNodeInfo1 n _) = n
        nodeInfo (ExplicitNodeInfo2 _ n) = n
        nodeInfo (ExplicitNodeInfo3 _ n _) = n
instance Pos ExplicitNodeInfo where
        posOf x = posOf (nodeInfo x)

instance CNode OneArgNodeInfo where
        nodeInfo (ExplicitNodeInfo4 _ n) = n
        nodeInfo (Delegator d) = nodeInfo d
instance Pos OneArgNodeInfo where
        posOf x = posOf (nodeInfo x)

instance CNode t1 => CNode (PolyVarNodeInfo t1) where
        nodeInfo (PolyCon2 _ _ n) = nodeInfo n
        nodeInfo (PolyCon1 _ n) = nodeInfo n
        nodeInfo (PolyCon0 d) = nodeInfo d
        nodeInfo (PolyDelegator d) = nodeInfo d
instance CNode t1 => Pos (PolyVarNodeInfo t1) where
        posOf x = posOf (nodeInfo x)
-- GENERATED STOP
