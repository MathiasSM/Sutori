{-|
  Description: Intermediate (TAC) code generation rules
-}

module Sutori.TAC.CodeGen where

import Control.Monad (void)
import Control.Monad.State (get, put)

import Sutori.AST
import Sutori.Monad
import Sutori.SymTable

import Sutori.TAC.TAC



-- |Appends to the TAC table a new triplet, references it on the instructions TAC table
--
-- Returns the Address of the inserted TAC
addTAC :: TAC -> SutMonad TACAddress
addTAC tac = do
  s@SutState{ tacNext = i
            , tacTable = TACTable{ tacInstructions = is
                                 , tacTriplets     = tacs } } <- get
  put s{ tacNext = i + 1
       , tacTable = TACTable{ tacInstructions = i:is
                            , tacTriplets     = tac:tacs } }
  return $ TACID i



-- Gets the next label
newLabel :: SutMonad TAC
newLabel = do
  s@SutState{ tacLabel = i } <- get
  put s{ tacLabel = i + 1 }
  return $ Label i



-- Generates code from the already built 'mainModule' AST
genCode :: SutMonad TACTable
genCode = do
  SutState{ mainModule = (SutModule _ m), parserTable = st } <- get
  genCodeAST (-1) (-1) m
  mapM_ genFunCode $ lookupAllFunctions 0 st
  SutState{ tacTable = t } <- get
  return t
  where
    genFunCode :: SymFunction -> SutMonad ()
    genFunCode (SymFunction fid _ _ a1 (Just a2)) = void $ do -- TODO: This fails badly if some function has no body
      newLabel
      addTAC $ FunLabel fid
      genCodeAST (-1) (-1) a1
      genCodeAST (-1) (-1) a2
      addTAC $ TAC Return Nothing Nothing



-- Generates code for a block of instructions (AST)
genCodeAST :: Int -> Int -> SutAST -> SutMonad ()
genCodeAST start' next' = mapM_ (genCodeInstr start' next')



-- |Generates code for instructions
genCodeInstr :: Int -> Int -> SutInstruction -> SutMonad ()

-- Expression Instruction
genCodeInstr _ _ (InstExpression expr) = void $ genCodeExpr expr

-- Return instructions
genCodeInstr _ _ (ReturnVal expr) = void $ do
  addr <- genCodeExpr expr
  addTAC $ TAC Return (Just addr) Nothing

-- Free Pointer
genCodeInstr _ _ (FreePointer _ expr) = void $ do
  exprAddr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysFree) (Just exprAddr) Nothing -- TODO: Specify FREE size?

-- Read IO
genCodeInstr _ _ (ReadVal _ expr) = void $ do
  addr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysRead) (Just addr) Nothing

-- Print IO
genCodeInstr _ _ (PrintVal _ expr) = void $ do
  addr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysPrint) (Just addr) Nothing

-- Break
genCodeInstr _ break' Break = void $ addTAC $ TAC Jump (Just $ TACLabel break') Nothing

-- Continue
genCodeInstr cont' _ Continue = void $ addTAC $ TAC Jump (Just $ TACLabel cont') Nothing


-- Selection / If-else
genCodeInstr cont' break' (Selection sid cond ifb elb) = void $ do
  ell@(Label ell') <- newLabel
  nex@(Label nex') <- newLabel

  condAddr <- genCodeExpr cond
  addTAC $ TAC JumpUnless (Just condAddr) (Just $ TACLabel ell')
  genCodeAST cont' break' ifb
  addTAC $ TAC Jump (Just $ TACLabel nex') Nothing

  addTAC ell
  genCodeAST cont' break' elb

  addTAC nex


-- IterationU / While
genCodeInstr _ _ (IterationU _ cond itb) = void $ do
  start@(Label start') <- newLabel
  next@(Label next')   <- newLabel

  addTAC start

  condAddr <- genCodeExpr cond
  addTAC $ TAC JumpUnless (Just condAddr) (Just $ TACLabel next')

  genCodeAST start' next' itb
  addTAC $ TAC Jump (Just $ TACLabel start') Nothing

  addTAC next


-- IterationB / For
genCodeInstr _ _ (IterationB _ idx itb) = void $ do
  start@(Label start') <- newLabel
  next@(Label next')   <- newLabel

  idxAddr <- genCodeExpr idx

  addTAC start
  addTAC $ TAC JumpUnless (Just idxAddr) (Just $ TACLabel next')

  genCodeAST start' next' itb
  mi <- addTAC $ TAC (Basic SutOpSub) (Just idxAddr) (Just $ TACLit $ SutInt 1)
  addTAC $ TAC (Basic SutOpAssign) (Just idxAddr) (Just mi)
  addTAC $ TAC Jump (Just $ TACLabel start') Nothing

  addTAC next



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

genCodeExpr (CreatePointer _ _) = addTAC $ TAC (SysCall SysAlloc) Nothing Nothing -- TODO: Specify ALLOC size

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
