module Sutori.MIPS.CodeGen where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (get)
import Data.Maybe (isJust, fromJust)

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


-- | Given a TAC instruction, outputs an equivalent sequence of MIPS instructions
genCode :: TAC -> SutMonad [MIPS]

genCode (Label i) = return [MLabel $ "LABEL_" ++ show i]
genCode (FunLabel i) = return [MLabel $ "FUN_" ++ i]

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
        genCodeBasic3 SutOpPow     (r0:r1:r2:_) = error "MIPS translation for Pow not implemented"
        genCodeBasic3 SutOpAnd     (r0:r1:r2:_) = [MAnd r0 r1 r2]
        genCodeBasic3 SutOpOr      (r0:r1:r2:_) = [MOr r0 r1 r2]
        genCodeBasic3 SutOpEqual   (r0:r1:r2:_) = [MEq r0 r1 r2]
        genCodeBasic3 SutOpNotEq   (r0:r1:r2:_) = [MNe r0 r1 r2]
        genCodeBasic3 SutOpGEq     (r0:r1:r2:_) = [MGe r0 r1 r2]
        genCodeBasic3 SutOpLEq     (r0:r1:r2:_) = [MLe r0 r1 r2]
        genCodeBasic3 SutOpGreater (r0:r1:r2:_) = [MGt r0 r1 r2]
        genCodeBasic3 SutOpLess    (r0:r1:r2:_) = [MLt r0 r1 r2]
        genCodeBasic3 SutOpAssign  (r1:r2:_)    = [MMove r1 r2]
        genCodeBasic3 SutOpIndex   (r0:r1:r2:_) = error "MIPS translation for Indexation not implemented"
        genCodeBasic3 SutOpMember  (r0:r1:r2:_) = error "MIPS translation for Member Access not implemented"

genCode TAC{tacType = SysCall sc, tac1 = t1, tac2 = t2} = genCodeSysCall sc t1 t2
  where genCodeSysCall :: SutSys -> Maybe TACAddress -> Maybe TACAddress -> SutMonad [MIPS]
        genCodeSysCall SysRead _ _  = error "MIPS translation for SysRead not implemented"
        genCodeSysCall SysPrint _ _ = error "MIPS translation for SysPrint not implemented"
        genCodeSysCall SysAlloc _ _ = error "MIPS translation for SysAlloc not implemented"
        genCodeSysCall SysFree _ _  = error "MIPS translation for SysFree not implemented"

genCode tac@TAC{tacType = Copy, tac1 = Just s, tac2 = moff} = do
  let regs@(r0:r1:r2:_) = getRegs tac
      off = if isJust moff then let (Just (TACLit (SutInt i))) = moff in i else 0
  load <- loadData (r1, s, off)
  let ops = genCodeCopy r0 r1
  return $ load ++ ops
  where genCodeCopy :: Reg -> Reg -> [MIPS]
        genCodeCopy r0 r1 = [MMove r0 r1]

genCode TAC{tacType = Addr, tac1 = Just base, tac2 = Just off} = genCodeAddr base off
  where genCodeAddr :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeAddr base off = error "MIPS translation for addressing not implemented"

genCode TAC{tacType = Call, tac1 = Just fid, tac2 = Just pn} = genCodeCall fid pn
  where genCodeCall :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeCall fid pn = error "MIPS translation of function call not implemented"

genCode TAC{tacType = Return, tac1 = ret} = genCodeReturn ret
  where genCodeReturn :: Maybe TACAddress -> SutMonad [MIPS]
        genCodeReturn Nothing  = error "MIPS translation of void return not implemented"
        genCodeReturn (Just a) = error "MIPS translation of valued return not implemented"

genCode TAC{tacType = Jump, tac1 = Just t} = genCodeJump t
  where genCodeJump :: TACAddress -> SutMonad [MIPS]
        genCodeJump (TACLabel l)    = return [MJump ("LABEL_" ++ show l)]
        genCodeJump (TACFun fid)    = return [MJump ("FUN_" ++ fid)]
        genCodeJump (TACID i)       = return $ error "MIPS spilled temps reuse for Jump not implemented"
        genCodeJump _               = return $ error "Unknown/undefined action on Jump"

genCode tac@TAC{tacType = JumpUnless, tac1 = Just cond, tac2 = Just labelAddr } = do
  let regs@(r0:_) = getRegs tac
  loads <- loadData (r0, cond, 0)
  ops   <- genCodeJumpUnless r0 labelAddr
  return $ loads ++ ops
  where genCodeJumpUnless :: Reg -> TACAddress -> SutMonad [MIPS]
        genCodeJumpUnless cond (TACLabel l)    = return [MBne RZ cond ("LABEL_" ++ show l)]
        genCodeJumpUnless cond (TACFun fid)    = return [MBne RZ cond ("FUN_" ++ fid)]
        genCodeJumpUnless cond (TACID i)       = return $ error "MIPS spilled temps reuse for JumpUnless not implemented"
        genCodeJumpUnless _ _                  = return $ error "Unknown/undefined action on JumpUnless"

genCode TAC{tacType = Pointed, tac1 = Just ptr} = genCodePointed ptr
  where genCodePointed :: TACAddress -> SutMonad [MIPS]
        genCodePointed ptr = error "MIPS translation of dereference not implemented"

genCode TAC{tacType = Param, tac1 = Just param} = genCodeParam param
  where genCodeParam :: TACAddress -> SutMonad [MIPS]
        genCodeParam _ = error "MIPS param passing not implemented"

-- | Given a TAC instruction, gives a list of registers to use for the equivalent MIPS instruction
getRegs :: TAC -> [Reg]
getRegs _ = error "MIPS register allocation not implemented"
