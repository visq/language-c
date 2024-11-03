{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Analysis.MachineDescs
  where

import Language.C.Analysis.ConstEval
import Language.C.Analysis.SemRep

x86_64 :: MachineDesc
x86_64 =
  let iSize = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 8
        TyULong   -> 8
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> 16
        TyUInt128 -> 16
      fSize = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 16
        TyBFloat16 -> error "TyBFloat16"
        TyFloatN{} -> error "TyFloatN"
      builtinSize = \case
        TyVaList -> 24
        TyAny    -> error "TyAny"
      ptrSize  = 8
      voidSize = 1
      iAlign   = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 8
        TyULong   -> 8
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> 16
        TyUInt128 -> 16
      fAlign = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 16
        TyBFloat16{} -> error "TyBFloat16"
        TyFloatN{} -> error "TyFloatN"
      builtinAlign = \case
        TyVaList -> 8
        TyAny    -> error "TyAny"
      ptrAlign  = 8
      voidAlign = 1
  in  MachineDesc { .. }

armv7l :: MachineDesc
armv7l =
  let iSize = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 4
        TyULong   -> 4
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> error "TyInt128 on armv7l"
        TyUInt128 -> error "TyUInt128 on armv7l"
      fSize = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 8
        TyBFloat16 -> 2
        TyFloatN{} -> error "TyFloatN"
      builtinSize = \case
        TyVaList -> 4
        TyAny    -> error "TyAny"
      ptrSize  = 4
      voidSize = 1
      iAlign   = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 4
        TyULong   -> 4
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> error "TyInt128 on armv7l"
        TyUInt128 -> error "TyUInt128 on armv7l"
      fAlign = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 8
        TyBFloat16 -> 2
        TyFloatN{} -> error "TyFloatN"
      builtinAlign = \case
        TyVaList -> 4
        TyAny    -> error "TyAny"
      ptrAlign  = 4
      voidAlign = 1
  in  MachineDesc { .. }
