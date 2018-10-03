{-|
  Description : Defines the Compiler Monad, the compilation state and general functions
-}
module Sutori.Monad where

import Data.Word (Word8)
import Data.Maybe
import Data.Semigroup
import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip

import Sutori.Lexer.Posn       (SutPosn, initialPosn)
import Sutori.Logger           (SutLogger(SutLogger), SutError(NoError), SutLog)
import Sutori.SymTable         (SymTable, Scope, SutSymCategory(CatType), SutSymOther(SymTypeDef), insert)
import Sutori.Types.Graph      (TypeGraph, initialTypeGraph, initialNextTypeID)
import Sutori.Types.Primitives (SutTypeID, SutPrimitive(SutTypeVoid), primitives)


-- |Monadic Lexer/Parser current state.
data SutState = SutState
  { lexerPosn       :: SutPosn       -- ^ Position at current input location
  , lexerInput      :: String        -- ^ Current input
  , lexerChar       :: Char          -- ^ Character before the input
  , lexerStateCode  :: Int           -- ^ Current startcode
  , lexerBytes      :: [Word8]       -- ^ Current bytes read
  , lexerDepth      :: Int           -- ^ Current comment nesting
  , lexerString     :: String        -- ^ Currently built string
  , lexerStringOn   :: Bool          -- ^ True if currently on an open strin
  , parserTable     :: SymTable      -- ^ The symtable
  , parserStack     :: [Scope]       -- ^ The scopes stack
  , parserScopes    :: Set.Set Scope -- ^ The set of open scopes
  , parserNextScope :: Scope         -- ^ The next scope ID to open
  , typesGraph      :: TypeGraph     -- ^ The constructed type graph
  , typesNextID     :: SutTypeID     -- ^ The next type ID to be introduced
  , logVerbose      :: Bool          -- ^ Set the output to verbose of not
  , errorCode       :: SutError }    -- ^ Current error code, if any

-- |Initial state of a Sutori parse/scan/run
initialSutoriState :: SutState
initialSutoriState = SutState
  { lexerPosn       = initialPosn
  , lexerInput      = ""
  , lexerChar       = '\n'
  , lexerBytes      = []
  , lexerStateCode  = 0
  , lexerDepth      = 0
  , lexerString     = ""
  , lexerStringOn   = False
  , parserTable     = Map.empty
  , parserStack     = [0]
  , parserScopes    = Set.insert 0 Set.empty
  , parserNextScope = 0
  , typesGraph      = initialTypeGraph
  , typesNextID     = initialNextTypeID
  , logVerbose      = False
  , errorCode       = NoError }

-- |The Sutori monad. Composes state, logging and exception handling
type SutMonad a = StateT SutState (WriterT SutLogger (Except (SutError, SutLog))) a


-- Regular Actions
---------------------------------------------------------------------------------------------------
-- |Run the monad with a given action
runSutMonad :: SutMonad a -> SutState -> Except (SutError, SutLog) ((a, SutState), SutLogger)
runSutMonad f a = runWriterT $ runStateT f a

-- |Inserts a new scope into the parse
insertScope :: SutMonad ()
insertScope = do
  oldState@SutState{parserNextScope = newScope, parserScopes = scopes, parserStack = stack} <- get
  let newSet = Set.insert newScope scopes
      newStack = newScope : stack
  put $ oldState { parserStack = newStack, parserNextScope = newScope + 1, parserScopes = newSet}

-- |Removes last scope from the parse
removeScope :: SutMonad ()
removeScope = do
  oldState@SutState{parserStack = stack, parserScopes = scopes} <- get
  let newSet = Set.delete (head stack) scopes
      newStack = tail stack
  put $ oldState { parserStack = newStack, parserScopes = newSet}

-- |Get the current open scope
parserCurrentScope :: SutState -> Scope
parserCurrentScope = head . parserStack

-- |Set the current error code for the compilation
setErrorCode :: SutError -> SutMonad ()
setErrorCode err = get >>= \s -> put s{ errorCode = err }



-- Utils
-- ================================================================================================

-- |Run a monadic action only if verbose if turned on
ifVerbose :: SutMonad () -> SutMonad ()
ifVerbose f = get >>= \SutState{logVerbose = v} -> when v f
