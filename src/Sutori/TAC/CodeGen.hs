{-|
  Description: Intermediate (TAC) code generation rules
-}

module Sutori.TAC.CodeGen where

import Control.Monad (void, foldM_)
import Control.Monad.State (get, put)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import qualified Data.Map as Map

import Sutori.AST
import Sutori.Monad
import Sutori.SymTable
import Sutori.Types
import Sutori.Parser.Symbols

import Sutori.TAC.TAC



-- |Appends to the TAC table a new triplet, references it on the instructions TAC table
--
-- Returns the Address of the inserted TAC
--
-- TODO: Figure out how to remove duplicated code.
addTAC :: TAC -> SutMonad TACAddress
addTAC tac@(Label id) = do
  s@SutState{ tacNext = i
            , tacTable = t@TACTable{ tacInstructions = is
                                   , tacTriplets     = tacs
                                   , tacLabels       = labels } } <- get
  put s{ tacNext = i + 1
       , tacTable = t { tacInstructions = i:is
                      , tacTriplets     = tac:tacs
                      , tacLabels       = Map.insert id i labels } }
  return $ TACID i
addTAC tac@(FunLabel sid) = do
  s@SutState{ tacNext = i
            , tacTable = t@TACTable{ tacInstructions = is
                                   , tacTriplets     = tacs
                                   , tacFunctions    = funs } } <- get
  put s{ tacNext = i + 1
       , tacTable = t { tacInstructions = i:is
                      , tacTriplets     = tac:tacs
                      , tacFunctions    = Map.insert sid i funs } }
  return $ TACID i
addTAC tac = do
  s@SutState{ tacNext = i
            , tacTable = t@TACTable{ tacInstructions = is
                                   , tacTriplets     = tacs } } <- get
  put s{ tacNext = i + 1
       , tacTable = t { tacInstructions = i:is
                      , tacTriplets     = tac:tacs } }
  return $ TACID i



-- |Gets the next label
newLabel :: SutMonad TAC
newLabel = do
  s@SutState{ tacLabel = i } <- get
  put s{ tacLabel = i + 1 }
  return $ Label i



-- |Generates code from the already built 'mainModule' AST
--
-- Generates in order: TAC for main/global code, then TACs for each function.
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



-- |Generates code for a block of instructions (AST)
genCodeAST :: Int -> Int -> SutAST -> SutMonad ()
genCodeAST start' next' = mapM_ (genCodeInstr start' next')



-- |Generates code for instructions
genCodeInstr :: Int -- ^ Label (ID) pointing to the first instruction in this block
             -> Int -- ^ Label (ID) pointing to the first instruction following this block
             -> SutInstruction -- ^ Currently processing instruction
             -> SutMonad ()


-- Expression Instruction.
--
-- Generates code to evaluate the expression
genCodeInstr _ _ (InstExpression expr) = void $ genCodeExpr expr


-- Return instruction.
--
-- Generates code to evaluate the returned expression.
-- Then generates code to call return on the result.
genCodeInstr _ _ (ReturnVal expr) = void $ do
  addr <- genCodeExpr expr
  addTAC $ TAC Return (Just addr) Nothing


-- Free Pointer.
--
-- Generates code to evaluate the expression,
-- then calls free on the given address for the type size.
genCodeInstr _ _ (FreePointer _ expr) = void $ do
  SutState{typesGraph = g} <- get
  let (Just (_,size)) = lookupTypeID (expressionType expr) g
  exprAddr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysFree) (Just exprAddr) (Just $ TACLit $ SutInt size)


-- Read IO.
--
-- Generates code to evaluate expression.
-- Makes syscall to read on given address.
genCodeInstr _ _ (ReadVal _ expr) = void $ do
  addr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysRead) (Just addr) Nothing


-- Print IO.
--
-- Generates code to evaluate expression.
-- Makes syscall to print given address.
--
-- Note: Nothing special done for non-raw types
genCodeInstr _ _ (PrintVal _ expr) = void $ do
  SutState{typesGraph = g} <- get
  let (Just (_,size)) = lookupTypeID (expressionType expr) g
  addr <- genCodeExpr expr
  addTAC $ TAC (SysCall SysPrint) (Just addr) (Just $ TACLit $ SutInt size)


-- Break.
--
-- Generates code to jump to the "next address following block".
genCodeInstr _ break' Break = void $ addTAC $ TAC Jump (Just $ TACLabel break') Nothing


-- Continue.
--
-- Generates code to jump to the "first address in code".
genCodeInstr cont' _ Continue = void $ addTAC $ TAC Jump (Just $ TACLabel cont') Nothing


-- Selection / If-else.
--
-- Generates code to evaluate condition, then jump to the "else" if condition is false.
-- Generates code for the "if" block, then jump to after the "else".
-- Generates code for the "else" block.
--
-- Adds appropriate labels for jumping.
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


-- IterationU / While.
--
-- Generates code to evaluate condition, then jump to the "next address after iterative block" if condition false.
-- Generates code for the iterative block, then jump to the first address, before evaluating the condition.
--
-- Adds appropriate labels for jumping.
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
--
-- Generates code toevaluate index expression.
-- Jump unless it is non-zero/false.
-- Generates code for iterative block.
-- Generates code to substract one (1) from the index expression.
-- Jumps to before the condition is checked (after it was initially evaluated)
--
-- Adds appropriate labels for jumping.
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
--
-- Each production returns the address (as temporal register) of the resulting expression
genCodeExpr :: SutExpression -> SutMonad TACAddress


-- Variables
--
-- Globals get referenced by name in the TAC. Locals get referenced by their offset, relative to the FP.
genCodeExpr (ExprID _ vid s) =
  case s of
     0 -> return $ TACGlobal vid
     _ -> do
       SutState{ parserTable = symTable } <- get
       let syms = lookupSymbolsVariable vid symTable
           msym = find ((== s) . symScope) syms
           offs = maybe (-1) symOffset msym
       return $ TACOffset offs

-- Literals
--
-- Copies a literal into a register
genCodeExpr (ExprLiteral _ lit) = addTAC $ TAC Copy (Just $ TACLit lit) Nothing

-- Binary operations
--
-- Generates code to evaluate each operand.
-- Generates instruction for the binary operation.
genCodeExpr (BinaryOp _ op op1 op2) = do
  a1 <- genCodeExpr op1
  a2 <- genCodeExpr op2
  addTAC $ TAC (Basic op) (Just a1) (Just a2)

-- Unary operations
--
-- Generates code to evaluate the operand.
-- Generates operation.
genCodeExpr (UnaryOp _ op op1) = do
  a <- genCodeExpr op1
  addTAC $ TAC (Basic op) (Just a) Nothing

-- Function calls
--
-- Generates code to evaluate each parameter.
-- Generate 'push' instruction for each parameter (into the parameter stack).
-- Generate 'call' instruction for the function with the given number of parameters.
genCodeExpr (SutCall _ fid ps) = do
  psa <- mapM genCodeExpr ps
  mapM_ (\(i,pa) -> addTAC $ TAC Param (Just pa) (Just $ TACLit $ SutInt i)) (zip [0..] psa)
  addTAC $ TAC Call (Just $ TACFun fid) (Just $ TACLit $ SutInt $ length ps)

-- Pointer creation / memory allocation
--
-- Gets the size required by the type.
-- Issues an 'alloc' syscall for said size.
genCodeExpr (CreatePointer t _) = do
  SutState{typesGraph = g} <- get
  let (Just (_,size)) = lookupTypeID t g
  addTAC $ TAC (SysCall SysAlloc) (Just $ TACLit $ SutInt size) Nothing

-- Dereference
--
-- Generates code to evaluate expression.
-- Generates instruction to get the pointed address.
genCodeExpr (Dereference _ expr) = do
  pt <- genCodeExpr expr
  addTAC $ TAC Pointed (Just pt) Nothing

-- Indexation
--
-- Generates code for array expression.
-- Generates code for index expression.
-- Gets element size and the array offset. TODO: What is the array offset???
-- Generates instructions to calculate element offset (iwc).
-- Generates instruction to get element from said offset.
genCodeExpr (ArrayGet t expr idx) = do
  arr <- genCodeExpr expr
  ida <- genCodeExpr idx
  SutState{typesGraph = g} <- get
  let (Just (_,elemSize)) = lookupTypeID t g
      arrOffset = Just $ TACLit $ SutInt (-1)
  iw <- addTAC $ TAC (Basic SutOpMul) (Just ida) (Just $ TACLit $ SutInt elemSize)
  iwc <- addTAC $ TAC (Basic SutOpAdd) (Just iw) arrOffset
  valA <- addTAC $ TAC Copy (Just arr) (Just iwc)
  addTAC $ TAC Copy (Just valA) Nothing

-- Get member
--
-- Generates code to evaluate struct expression.
-- Gets the member size and offset. TODO: Should take the struct offset into account too!
-- Generates code to get said size from said offset.
genCodeExpr (MemberGet t expr mid) = do
  str <- genCodeExpr expr      -- Evaluate struct expression
  SutState{typesGraph = g} <- get
  let (Just (_,elemSize)) = lookupTypeID t g  -- Get the member type size
      offset = memberOffset t mid             -- Get member offset
  addTAC $ TAC Copy (Just str) (Just $ TACLit $ SutInt offset) -- Copy of said size from the offset

-- Syntactic arrays
--
-- Get the element and array sizes.
-- Generate code to alloc memory for the array. TODO: Shouldn't this be stacked?
-- Generate code to evaluate each element and copy to the corresponding offset from array position.
genCodeExpr (ExprConstructor (SutChain _ t) (SutArray elems)) = do
  SutState{typesGraph = g} <- get
  let (Just (_,elemSize)) = lookupType t g  -- Get the element type size
      arrSize = length elems * elemSize
  arrA <- addTAC $ TAC (SysCall SysAlloc) (Just $ TACLit $ SutInt arrSize) Nothing -- Request mem for array
  mapM_ (addElem arrA) (zip (map (elemSize *) [0..]) elems)             -- Generate code for each element and 'append' to array
  return arrA
  where addElem arrA (pos, elem) = do
          elemA <- genCodeExpr elem
          posA <- addTAC $ TAC Addr (Just arrA) (Just $ TACLit $ SutInt pos)
          addTAC $ TAC (Basic SutOpAssign) (Just posA) (Just elemA)

-- Syntactic structs
--
-- Gets the struct size. TODO: Ha, problems with `struct A { a: A }`.
-- Generates code to alloc said size of memory.
-- Generates code to copy each member of the struct to the struct, given its offset.
genCodeExpr (ExprConstructor t (SutStruct members)) = do
  SutState{typesGraph = g} <- get
  let (Just (_,strSize)) = lookupTypeID t g  -- Get the element type size

  strA <- addTAC $ TAC (SysCall SysAlloc) (Just $ TACLit $ SutInt strSize) Nothing -- Request mem for struct
  mapM_ (addMember strA) members             -- Generate code for each member and append to struct
  return strA
  where addMember strA (mid, mexpr) = do
          let offset = memberOffset t mid   -- Get member offset
          memA <- genCodeExpr mexpr         -- Generate code for member expression
          posA <- addTAC $ TAC Addr (Just strA) (Just $ TACLit $ SutInt offset) -- Generate code to get member position
          addTAC $ TAC (Basic SutOpAssign) (Just posA) (Just memA)              -- Generate code to copy member
