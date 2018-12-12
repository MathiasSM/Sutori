module Sutori.MIPS.CodeGen where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad ((>=>))
import Control.Monad.State (get)
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector as Vector

import Sutori.TAC.TAC
import Sutori.TAC.ControlFlow
import Sutori.Monad
import Sutori.AST

import Sutori.MIPS.MIPS


loadData :: (Reg, TACAddress, Offset) -> SutMonad [MIPS]
loadData (r, t, o) = do
  SutState{mipsRegDescriptors = regDs, mipsVarDescriptors = varDs} <- get
  let regD    = Map.lookup r regDs
      dExists = isJust regD
      isInReg = dExists && Set.member t (fromJust regD)
  if isInReg then return []
             else case t of
                    TACGlobal vid     -> return [MLoadWord r $ MemLabel vid o]
                    TACOffset off     -> return [MLoadWord r $ MemReg FP (off+o)]
                    TACLabel l        -> return [MLoadWord r $ MemLabel ("LABEL_" ++ show l) o]
                    TACID i           -> return $ error "MIPS spilled temps reuse not implemented"
                    TACFun fid        -> return [MLoadWord r $ MemLabel ("FUN_" ++ fid) o]
                    TACLit (SutInt l) -> return [MLoadImmediate r l]


genMIPS :: SutMonad [MIPS]
genMIPS = do
  SutState{tacTable = tt} <- get
  let flow = flowGraph tt
      nodes = flowNodes tt flow
  mipsNodes <- mapM (Vector.mapM genCode >=> (return .concat)) nodes
  return $ concat mipsNodes



-- | Given a TAC instruction, outputs an equivalent sequence of MIPS instructions
genCode :: TAC -> SutMonad [MIPS]

-- DONE?
genCode (Label i) = return [MLabel $ "LABEL_" ++ show i]
genCode (FunLabel i) = return [MLabel $ "FUN_" ++ i]

-- DONE? For the most part
genCode tac@TAC{tacType = Basic op, tac1 = Just t1, tac2 = mt2} =
  if isJust mt2
     then do
       let regs@(r0:r1:r2:_) = getRegs tac
           t2 = fromJust mt2
       loads <- mapM loadData (zip3 [r1, r2] [t1, t2] (repeat 0))
       let ops = genCodeBasic3 op regs
       return $ concat loads ++ ops
     else do
       let regs@(r0:r1:r2:_) = getRegs tac
       loads <- mapM loadData (zip3 [r1] [t1] (repeat 0))
       let ops = genCodeBasic2 op regs
       return $ concat loads ++ ops
  where genCodeBasic2 :: SutOperator -> [Reg] -> [MIPS]
        genCodeBasic2 SutOpNeg (r0:r1:_) = [MNeg r0 r1]
        genCodeBasic2 SutOpNot (r0:r1:_) = [MNot r0 r1]
        genCodeBasic2 SutOpPos _         = [MNoop]
        genCodeBasic2 SutOpDer (r0:r1:_) = [MLoadWord r0 (MemReg r1 0)]

        genCodeBasic3 :: SutOperator -> [Reg] -> [MIPS]
        genCodeBasic3 SutOpAdd     (r0:r1:r2:_) = [MAdd r0 r1 r2]
        genCodeBasic3 SutOpSub     (r0:r1:r2:_) = [MSub r0 r1 r2]
        genCodeBasic3 SutOpMul     (r0:r1:r2:_) = [MMul r0 r1 r2]
        genCodeBasic3 SutOpDiv     (r0:r1:r2:_) = [MDivS r0 r1 r2]
        genCodeBasic3 SutOpIntDiv  (r0:r1:r2:_) = [MDiv r0 r1 r2]
        genCodeBasic3 SutOpMod     (r0:r1:r2:_) = [MRem r0 r1 r2]
        genCodeBasic3 SutOpAnd     (r0:r1:r2:_) = [MAnd r0 r1 r2]
        genCodeBasic3 SutOpOr      (r0:r1:r2:_) = [MOr r0 r1 r2]
        genCodeBasic3 SutOpEqual   (r0:r1:r2:_) = [MEq r0 r1 r2]
        genCodeBasic3 SutOpNotEq   (r0:r1:r2:_) = [MNe r0 r1 r2]
        genCodeBasic3 SutOpGEq     (r0:r1:r2:_) = [MGe r0 r1 r2]
        genCodeBasic3 SutOpLEq     (r0:r1:r2:_) = [MLe r0 r1 r2]
        genCodeBasic3 SutOpGreater (r0:r1:r2:_) = [MGt r0 r1 r2]
        genCodeBasic3 SutOpLess    (r0:r1:r2:_) = [MLt r0 r1 r2]
        genCodeBasic3 SutOpAssign  (r1:r2:_)    = [MMove r1 r2]
        genCodeBasic3 SutOpPow     (r0:r1:r2:_) = error "MIPS translation for Pow not implemented"
        genCodeBasic3 SutOpIndex   (r0:r1:r2:_) = error "MIPS translation for Indexation not implemented"
        genCodeBasic3 SutOpMember  (r0:r1:r2:_) = error "MIPS translation for Member Access not implemented"

-- DONE?
genCode tac@TAC{tacType = Copy, tac1 = Just s, tac2 = moff} = do
  let regs@(r0:r1:r2:_) = getRegs tac
      off = if isJust moff then let (Just (TACLit (SutInt i))) = moff in i else 0
  load <- loadData (r1, s, off)
  let ops = genCodeCopy r0 r1
  return $ load ++ ops
  where genCodeCopy :: Reg -> Reg -> [MIPS]
        genCodeCopy r0 r1 = [MMove r0 r1]

-- DONE? More than 4 arguments is kaput
genCode tac@TAC{tacType = Param, tac1 = Just param, tac2 = Just (TACLit (SutInt i))} = do
  let regs@(an:r:_) = getRegs tac
  load <- loadData (r, param, 0)
  ops <- if i < 4 then genCodeRegParam an r else genCodeMemParam r
  return $ load ++ ops
  where genCodeRegParam :: Reg -> Reg -> SutMonad [MIPS]
        genCodeRegParam a r = return [MMove a r]

        genCodeMemParam :: Reg -> SutMonad [MIPS]
        genCodeMemParam r = error "MIPS param passing through mem locations not implemented"

-- DONE? Not handling >4 arguments
genCode TAC{tacType = Call, tac1 = Just (TACFun fid), tac2 = Just (TACLit (SutInt pn))} = genCodeCall fid pn
  where genCodeCall :: String -> Int -> SutMonad [MIPS]
        genCodeCall fid pn = return [MJumpAndLink fid] -- TODO: Handle >4 args!!

-- DONE? I assume size 32
genCode tac@TAC{tacType = Return, tac1 = ret} = do
  let regs@(an:r:_) = getRegs tac
  if isJust ret then genCodeReturnValue r else genCodeReturnVoid
  where genCodeReturnValue :: Reg -> SutMonad [MIPS]
        genCodeReturnValue r = do
          load <- loadData (r, fromJust ret, 0)
          return $ load ++ [MMove V0 r, MJumpReg RA]

        genCodeReturnVoid :: SutMonad [MIPS]
        genCodeReturnVoid = return [MJumpReg RA]

-- DONE?
genCode TAC{tacType = Jump, tac1 = Just t} = genCodeJump t
  where genCodeJump :: TACAddress -> SutMonad [MIPS]
        genCodeJump (TACLabel l)    = return [MJump ("LABEL_" ++ show l)]
        genCodeJump (TACFun fid)    = return [MJump ("FUN_" ++ fid)]
        genCodeJump _               = return $ error "Unknown/undefined action on Jump"

-- DONE?
genCode tac@TAC{tacType = JumpUnless, tac1 = Just cond, tac2 = Just labelAddr } = do
  let regs@(r0:_) = getRegs tac
  loads <- loadData (r0, cond, 0)
  ops   <- genCodeJumpUnless r0 labelAddr
  return $ loads ++ ops
  where genCodeJumpUnless :: Reg -> TACAddress -> SutMonad [MIPS]
        genCodeJumpUnless cond (TACLabel l)    = return [MBeq RZ cond ("LABEL_" ++ show l)] --TODO: Bne or Beq?
        genCodeJumpUnless cond (TACFun fid)    = return [MBeq RZ cond ("FUN_" ++ fid)] --TODO: Bne or Beq?
        genCodeJumpUnless _ _                  = return $ error "Unknown/undefined action on JumpUnless"

-- TODO
genCode TAC{tacType = SysCall sc, tac1 = t1, tac2 = t2} = genCodeSysCall sc t1 t2
  where genCodeSysCall :: SutSys -> Maybe TACAddress -> Maybe TACAddress -> SutMonad [MIPS]
        genCodeSysCall SysRead _ _  = error "MIPS translation for SysRead not implemented"
        genCodeSysCall SysPrint _ _ = error "MIPS translation for SysPrint not implemented"
        genCodeSysCall SysAlloc _ _ = error "MIPS translation for SysAlloc not implemented"
        genCodeSysCall SysFree _ _  = error "MIPS translation for SysFree not implemented"

-- TODO
genCode TAC{tacType = Pointed, tac1 = Just ptr} = genCodePointed ptr
  where genCodePointed :: TACAddress -> SutMonad [MIPS]
        genCodePointed ptr = error "MIPS translation of dereference not implemented"

-- TODO: This shouldn't be necessary if proper handling of arrays in TACs
genCode TAC{tacType = Addr, tac1 = Just base, tac2 = Just off} = genCodeAddr base off
  where genCodeAddr :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeAddr base off = error "MIPS translation for addressing not implemented"


-- | Given a TAC instruction, gives a list of registers to use for the equivalent MIPS instruction
--
-- TODO
getRegs :: TAC -> [Reg]
getRegs _ = error "MIPS register allocation not implemented"
