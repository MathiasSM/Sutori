module Sutori.Types.Primitives
( SutPrimitive(..)
, SutTypeID
, primitives
, generalizePrimitives
, toTypeLight
, toTypeNum
, toTypeBag
, toTypeWallet
) where

import Data.Graph

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))


-- A type will be represented by an ID
type SutTypeID = Int

-- Sutori primitives follow (their IDs are presented below)
data SutPrimitive = SutBag
                  | SutWallet
                  | SutPhrase
                  | SutLight
                  | SutLetter
                  | SutTypeVoid
                  | SutTypeError
                  deriving (Eq, Ord)

instance SutShow SutPrimitive where
  showSut SutBag           = SutLogLeave "Bag"
  showSut SutWallet        = SutLogLeave "Wallet"
  showSut SutPhrase        = SutLogLeave "Phrase"
  showSut SutLight         = SutLogLeave "Light"
  showSut SutLetter        = SutLogLeave "Letter"
  showSut SutTypeVoid      = SutLogLeave "No Type"
  showSut SutTypeError     = SutLogLeave "Type error"

-- Predefined Sutori types to initialize symtable
primitives :: [SutPrimitive]
primitives = map (\(c,_,_) -> c) primitiveEdges

-- Eventual TypeIDs for the type graph
primitiveID :: SutPrimitive -> SutTypeID
primitiveID SutTypeError = 0
primitiveID SutTypeVoid  = 1
primitiveID SutLight     = 2
primitiveID SutLetter    = 3
primitiveID SutBag       = 4
primitiveID SutWallet    = 5
primitiveID SutPhrase    = 6

-- Graph for primitives generalization
primitiveEdges =
  [ (SutTypeError, primitiveID SutTypeError, []                 )
  , (SutTypeVoid,  primitiveID SutTypeVoid,  [primitiveID SutLight]  )
  , (SutLight,     primitiveID SutLight,     [primitiveID SutLetter] )
  , (SutLetter,    primitiveID SutLetter,    [primitiveID SutBag]    )
  , (SutBag,       primitiveID SutBag,       [primitiveID SutWallet] )
  , (SutWallet,    primitiveID SutWallet,    []                 )
  , (SutPhrase,    primitiveID SutPhrase,    []                 )
  ]

(primitiveG, _, _) = graphFromEdges primitiveEdges


-- From two types, return the most general one (Right now LCA is just always one of them)
generalizePrimitives :: SutPrimitive -> SutPrimitive -> SutPrimitive
generalizePrimitives SutTypeError _ = SutTypeError
generalizePrimitives _ SutTypeError = SutTypeError
generalizePrimitives t1 t2 = let reaches = path primitiveG
                                 v1 = primitiveID t1
                                 v2 = primitiveID t2
                              in if v1 `reaches` v2
                                    then t1
                                    else if v2 `reaches` v1
                                         then t2
                                         else SutTypeError

-- Go to a boolean type
toTypeLight :: SutPrimitive -> SutPrimitive
toTypeLight SutTypeVoid = SutLight
toTypeLight SutLight    = SutLight
toTypeLight SutLetter   = SutLight
toTypeLight SutBag      = SutLight
toTypeLight _           = SutTypeError

-- Go to the most specific numerical type available
toTypeBag :: SutPrimitive -> SutPrimitive
toTypeBag SutTypeVoid = SutBag
toTypeBag SutLight    = SutBag
toTypeBag SutLetter   = SutBag
toTypeBag SutBag      = SutBag
toTypeBag SutWallet   = SutBag
toTypeBag _           = SutTypeError

-- Go to the most specific numerical type available
toTypeNum :: SutPrimitive -> SutPrimitive
toTypeNum SutTypeVoid = SutBag
toTypeNum SutLight    = SutBag
toTypeNum SutLetter   = SutBag
toTypeNum SutBag      = SutBag
toTypeNum SutWallet   = SutWallet
toTypeNum _           = SutTypeError

-- Go to the general float type
toTypeWallet :: SutPrimitive -> SutPrimitive
toTypeWallet SutTypeVoid = SutWallet
toTypeWallet SutLight    = SutWallet
toTypeWallet SutLetter   = SutWallet
toTypeWallet SutBag      = SutWallet
toTypeWallet SutWallet   = SutWallet
toTypeWallet _           = SutTypeError
