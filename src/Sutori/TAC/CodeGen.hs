{-|
  Description: Intermediate (TAC) code generation rules
-}

module Sutori.TAC.CodeGen where

import Control.Monad.State (get, put)
import Data.Maybe (fromJust)

import Sutori.AST
import Sutori.Monad
import Sutori.SymTable

import Sutori.TAC.TAC


-- |Appends to the TAC table a new triplet, references it on the instructions TAC table
--
-- Returns the Address of the inserted TAC
addTAC :: TAC -> SutMonad TACAddress
addTAC tac = do
  s@SutState{ tacNext = i, tacTable = TACTable{ tacInstructions = is, tacTriplets = tacs } } <- get
  put s{ tacNext = i + 1, tacTable = TACTable{tacInstructions = i:is, tacTriplets = tac:tacs} }
  return $ TACID i



-- -- Updates an "old" instruction with a new TAC (Backpatching)
-- --
-- -- More commonly used to update Jumps with now-known addresses
-- updateCode :: TACAddress -> TAC -> SutMonad TACAddress
-- updateCode ja@(TACID j) tac = do
--   s@SutState{ tacNext = i, tacTable = t@TACTable{ tacTriplets = tacs } } <- get
--   let idx    = i - j
--       (bnew, _:bold) = splitAt idx tacs -- Hopefuly not too far from the present
--       tacs' = bnew ++ tac:bold
--   put s{tacTable = t{tacTriplets = tacs'}}
--   return ja



-- Gets the next label
nextLabel :: SutMonad TACAddress
nextLabel = get >>= \SutState{ tacNext = i } -> return (TACID i)


-- Wraps a code-generation monadic action to return the first and next addresses
genCodeWrap :: SutMonad a -> SutMonad (TACAddress, TACAddress)
genCodeWrap a = nextLabel >>= \f -> a >> nextLabel >>= \n -> return (f,n)


-- Generates code from the already built 'mainModule' AST
genCode :: SutMonad TACTable
genCode = do
  SutState{ mainModule = (SutModule _ m), parserTable = st } <- get
  let fs = map (\(SymFunction fid _ _ a1 a2) -> (fid, (a1, a2))) $ lookupAllFunctions 0 st
  mapM_ genCodeAST [n | (a,b) <- map snd fs, n <- [a, fromJust b]] -- TODO: This fails badly if some function has no body
  genCodeAST m
  SutState{ tacTable = t } <- get
  return t


-- Generates code for a block of instructions (AST)
--
-- Returns the first address and next address in a pair
genCodeAST :: SutAST -> SutMonad (TACAddress, TACAddress)
genCodeAST ast = genCodeWrap $ mapM_ genCodeInstr ast


-- |Generates code for instructions
--
-- Each production returns the first and next instruction addresses
genCodeInstr :: SutInstruction -> SutMonad (TACAddress, TACAddress)

-- Expression Instruction
genCodeInstr (InstExpression expr) = genCodeWrap $ genCodeExpr expr


-- Return instructions
genCodeInstr (ReturnVal expr) = genCodeWrap $ do
  addr <- genCodeExpr expr
  addTAC $ TAC Return (Just addr) Nothing


-- Selection / If-else
genCodeInstr (Selection sid cond ifb elb) = genCodeWrap $ do
  coAddr <- genCodeExpr cond
  jump1 <- addTAC $ TAC JumpUnless (Just coAddr) Nothing

  (ifAddr, _) <- genCodeAST ifb
  jump2 <- addTAC $ TAC Jump Nothing Nothing

  -- (elAddr, l)  <-
  genCodeAST elb

  -- TODO: We need to update / backpatch the incomplete jump instructions
  -- updateCode jump1 (TAC JumpUnless (Just coAddr) (Just elAddr))
  -- updateCode jump2 (TAC Jump       (Just l) Nothing)


-- IterationU / While
genCodeInstr (IterationU _ cond itb) = genCodeWrap $ do
  coAddr <- genCodeExpr cond
  jump1 <- addTAC $ TAC JumpUnless (Just coAddr) Nothing

  genCodeAST itb
  addTAC $ TAC Jump (Just coAddr) Nothing

  -- l  <- nextLabel

  -- TODO: We need to update / backpatch the incomplete jump instructions
  -- updateCode jump1 (TAC JumpUnless (Just coAddr) (Just l))


-- IterationB / For
genCodeInstr (IterationB _ idx itb) = genCodeWrap $ do
  idxAddr <- genCodeExpr idx
  jump1 <- addTAC $ TAC JumpUnless (Just idxAddr) Nothing

  genCodeAST itb
  mi <- addTAC $ TAC (Basic SutOpSub) (Just idxAddr) (Just $ TACLit $ SutInt 1)
  addTAC $ TAC (Basic SutOpAssign) (Just idxAddr) (Just mi)
  addTAC $ TAC Jump (Just idxAddr) Nothing

  -- l  <- nextLabel

  -- TODO: We need to update / backpatch the incomplete jump instructions
  -- updateCode jump1 (TAC JumpUnless (Just idxAddr) (Just l))


-- Free Pointer
genCodeInstr (FreePointer _ expr) = genCodeWrap $ do
  exprAddr <- genCodeExpr expr
  addTAC $ TAC SysCall (Just exprAddr) Nothing -- TODO: Specify FREE instruction


-- Read IO
genCodeInstr (ReadVal _ expr) = genCodeWrap $ do
  addr <- genCodeExpr expr
  addTAC $ TAC SysCall (Just addr) Nothing -- TODO: Specify READ instruction

-- Print IO
genCodeInstr (PrintVal _ expr) = genCodeWrap $ do
  addr <- genCodeExpr expr
  addTAC $ TAC SysCall (Just addr) Nothing -- TODO: Specify PRINT instruction

-- Break
genCodeInstr Break = genCodeWrap $ do
  addTAC $ TAC Jump Nothing Nothing
  -- We need to update / backpatch the incomplete jump instructions
  -- TODO: We cannot do this here, must do it offline

-- Continue
genCodeInstr Continue = genCodeWrap $ do
  addTAC $ TAC Jump Nothing Nothing
  -- We need to update / backpatch the incomplete jump instructions
  -- TODO: We cannot do this here, must do it offline




-- |Generates code for expressions
genCodeExpr :: SutExpression -> SutMonad TACAddress

genCodeExpr (ExprID _ vid s) = return $ TACName (vid, s)

genCodeExpr (ExprLiteral _ lit) = addTAC $ TAC Copy (Just $ TACLit lit) Nothing

genCodeExpr (BinaryOp _ op op1 op2) = do
  a1 <- genCodeExpr op1
  a2 <- genCodeExpr op2
  addTAC $ TAC (Basic op) (Just a1) (Just a2)

genCodeExpr (UnaryOp _ op op1) = do
  a <- genCodeExpr op1
  addTAC $ TAC (Basic op) (Just a) Nothing

genCodeExpr (SutCall _ fid ps) = do
  psa <- mapM genCodeExpr ps                               -- We evaluate the expression, get their final addresses
  mapM_ (\pa -> addTAC $ TAC Param (Just pa) Nothing) psa  -- We stack the parameters
  addTAC $ TAC Call (Just $ TACName (fid, 0)) (Just $ TACLit $ SutInt $ length ps)

genCodeExpr (CreatePointer _ _) = addTAC $ TAC SysCall Nothing Nothing -- TODO: Specify ALLOC instruction

genCodeExpr (Dereference _ expr) = do
  pt <- genCodeExpr expr
  addTAC $ TAC Pointed (Just pt) Nothing

genCodeExpr (ArrayGet _ expr idx) = do
  arr <- genCodeExpr expr
  ida <- genCodeExpr idx
  posA <- addTAC $ TAC Array (Just arr) (Just ida)
  addTAC $ TAC Copy (Just posA) Nothing

genCodeExpr (MemberGet _ expr mid) = error ""

genCodeExpr (ExprConstructor _ (SutArray elems)) = do
  addrs <- mapM genCodeExpr elems
  arrA <- addTAC $ TAC Copy Nothing Nothing
  mapM_ (addElem arrA) (zip [0..] addrs)
  return arrA
  where
    addElem arr (idx, valueA) = do
      posA <- addTAC $ TAC Array (Just arr) (Just $ TACLit $ SutInt idx)
      addTAC $ TAC (Basic SutOpAssign) (Just posA) (Just valueA)

genCodeExpr (ExprConstructor _ (SutStruct members)) = error ""
