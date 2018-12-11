module Sutori.MIPS.CodeGen where

import Sutori.TAC.TAC
import Sutori.TAC.ControlFlow
import Sutori.Monad
import Sutori.AST

import Sutori.MIPS.MIPS

-- | Given a TAC instruction, outputs an equivalent sequence of MIPS instructions
genCode :: TAC -> SutMonad [MIPS]

genCode (Label i) = return [MLabel $ "LABEL_" ++ show i]
genCode (FunLabel i) = return [MLabel $ "FUN_" ++ i]

genCode TAC{tacType = Basic op, tac1 = t1, tac2 = t2} = genCodeBasic op t1 t2
  where genCodeBasic :: SutOperator -> Maybe TACAddress -> Maybe TACAddress -> SutMonad [MIPS]
        genCodeBasic SutOpNeg (Just t1) Nothing       = error "MIPS translation for Neg not implemented"
        genCodeBasic SutOpNot (Just t1) Nothing       = error "MIPS translation for Not not implemented"
        genCodeBasic SutOpPos (Just t1) Nothing       = error "MIPS translation for Pos not implemented"
        genCodeBasic SutOpDer (Just t1) Nothing       = error "MIPS translation for Der not implemented"
        genCodeBasic SutOpAdd (Just t1) (Just t2)     = error "MIPS translation for Add not implemented"
        genCodeBasic SutOpSub (Just t1) (Just t2)     = error "MIPS translation for Sub not implemented"
        genCodeBasic SutOpMul (Just t1) (Just t2)     = error "MIPS translation for Mul not implemented"
        genCodeBasic SutOpDiv (Just t1) (Just t2)     = error "MIPS translation for Div not implemented"
        genCodeBasic SutOpIntDiv (Just t1) (Just t2)  = error "MIPS translation for IntDiv not implemented"
        genCodeBasic SutOpMod (Just t1) (Just t2)     = error "MIPS translation for Mod not implemented"
        genCodeBasic SutOpPow (Just t1) (Just t2)     = error "MIPS translation for Pow not implemented"
        genCodeBasic SutOpAnd (Just t1) (Just t2)     = error "MIPS translation for And not implemented"
        genCodeBasic SutOpOr (Just t1) (Just t2)      = error "MIPS translation for Or not implemented"
        genCodeBasic SutOpEqual (Just t1) (Just t2)   = error "MIPS translation for Equal not implemented"
        genCodeBasic SutOpNotEq (Just t1) (Just t2)   = error "MIPS translation for NotEq not implemented"
        genCodeBasic SutOpGEq (Just t1) (Just t2)     = error "MIPS translation for GEq not implemented"
        genCodeBasic SutOpLEq (Just t1) (Just t2)     = error "MIPS translation for LEq not implemented"
        genCodeBasic SutOpGreater (Just t1) (Just t2) = error "MIPS translation for Greater not implemented"
        genCodeBasic SutOpLess (Just t1) (Just t2)    = error "MIPS translation for Less not implemented"
        genCodeBasic SutOpAssign (Just t1) (Just t2)  = error "MIPS translation for Assign not implemented"
        genCodeBasic SutOpIndex (Just t1) (Just t2)   = error "MIPS translation for Index not implemented"
        genCodeBasic SutOpMember (Just t1) (Just t2)  = error "MIPS translation for Member not implemented"

genCode TAC{tacType = SysCall sc, tac1 = t1, tac2 = t2} = genCodeSysCall sc t1 t2
  where genCodeSysCall :: SutSys -> Maybe TACAddress -> Maybe TACAddress -> SutMonad [MIPS]
        genCodeSysCall SysRead _ _  = error "MIPS translation for SysRead not implemented"
        genCodeSysCall SysPrint _ _ = error "MIPS translation for SysPrint not implemented"
        genCodeSysCall SysAlloc _ _ = error "MIPS translation for SysAlloc not implemented"
        genCodeSysCall SysFree _ _  = error "MIPS translation for SysFree not implemented"

genCode TAC{tacType = Copy, tac1 = Just s, tac2 = off} = genCodeCopy s off
  where genCodeCopy :: TACAddress -> Maybe TACAddress -> SutMonad [MIPS]
        genCodeCopy s (Just off) = error "MIPS translation for offset Copy not implemented"
        genCodeCopy s Nothing    = error "MIPS translation for direct Copy not implemented"

genCode TAC{tacType = Addr, tac1 = Just base, tac2 = Just off} = genCodeAddr base off
  where genCodeAddr :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeAddr base off = error "MIPS translation for addressing not implemented"

genCode TAC{tacType = Return, tac1 = ret} = genCodeReturn ret
  where genCodeReturn :: Maybe TACAddress -> SutMonad [MIPS]
        genCodeReturn Nothing  = error "MIPS translation of void return not implemented"
        genCodeReturn (Just a) = error "MIPS translation of valued return not implemented"

genCode TAC{tacType = Call, tac1 = Just fid, tac2 = Just pn} = genCodeCall fid pn
  where genCodeCall :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeCall fid pn = error "MIPS translation of function call not implemented"

genCode TAC{tacType = Jump, tac1 = Just t} = genCodeJump t
  where genCodeJump :: TACAddress -> SutMonad [MIPS]
        genCodeJump t = error "MIPS translation of inconditional jumps not implemented"

genCode TAC{tacType = JumpUnless, tac1 = Just cond, tac2 = Just t } = genCodeJumpUnless cond t
  where genCodeJumpUnless :: TACAddress -> TACAddress -> SutMonad [MIPS]
        genCodeJumpUnless cond t = error "MIPS translation of conditional jump not implemented"

genCode TAC{tacType = Pointed, tac1 = Just ptr} = genCodePointed ptr
  where genCodePointed :: TACAddress -> SutMonad [MIPS]
        genCodePointed ptr = error "MIPS translation of dereference not implemented"

genCode TAC{tacType = Param, tac1 = Just param} = genCodeParam param
  where genCodeParam :: TACAddress -> SutMonad [MIPS]
        genCodeParam _ = error "MIPS param passing not implemented"

-- | Given a TAC instruction, gives a list of registers to use for the equivalent MIPS instruction
getReg :: TAC -> [Reg]
getReg _ = []
