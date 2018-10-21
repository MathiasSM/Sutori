{-|
  Description : The compilation state
-}
module Sutori.Monad.State where

import Data.Word (Word8)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Sutori.AST         (SutModule)
import Sutori.Lexer.Posn  (SutPosn, initialPosn)
import Sutori.Logger      (SutError(NoError))
import Sutori.SymTable    (SymTable, Scope)
import Sutori.Types       (SutTypeID, TypeGraph, initialTypeGraph, initialNextTypeID)


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
  , mainModule      :: SutModule     -- ^ The main module (where compilation began)
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
  , mainModule      = undefined
  , typesGraph      = initialTypeGraph
  , typesNextID     = initialNextTypeID
  , logVerbose      = False
  , errorCode       = NoError }

-- |Get the current open scope
parserCurrentScope :: SutState -> Scope
parserCurrentScope = head . parserStack
