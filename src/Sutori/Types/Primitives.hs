{-|
Description : Defines Sutori primitive types, build a generalization graph fro them
              and provides functions to get the most general type of two primitives
-}
module Sutori.Types.Primitives
( SutPrimitive(..)
, SutTypeID
, primitives
, primitiveSize
, primitiveID
, primitiveIDs
, generalizePrimitives
, toTypeLight
, toTypeNum
, toTypeBag
, toTypeWallet
, toTypePhrase
, toTypeSortable
, toTypeEq
) where

import Data.Graph
import Data.Maybe(fromJust)


-- |A type will be represented by an ID
type SutTypeID = Int

-- |Sutori primitives
data SutPrimitive = SutBag        -- ^ A bag (Int)
                  | SutWallet     -- ^ A wallet (Float)
                  | SutPhrase     -- ^ A phrase (String)
                  | SutLight      -- ^ A light (Bool)
                  | SutLetter     -- ^ A letter (Char)
                  | SutTypeVoid   -- ^ A void element (Void)
                  | SutTypeError  -- ^ A Type Error (&^%$#)
  deriving (Eq, Ord, Show)

-- |Predefined Sutori types to initialize symtable
primitives :: [SutPrimitive]
primitives =
  [ SutTypeError
  , SutTypeVoid
  , SutLight
  , SutLetter
  , SutBag
  , SutWallet
  , SutPhrase ]

-- |Defines sizes for each primitive ("in bytes"-ish)
primitiveSize :: SutPrimitive -> Int
primitiveSize SutTypeError = 0
primitiveSize SutTypeVoid  = 0
primitiveSize SutLight     = 1
primitiveSize SutLetter    = 1
primitiveSize SutBag       = 4
primitiveSize SutWallet    = 8 -- Double precision
primitiveSize SutPhrase    = 4 -- Basically a pointer to the actual string


-- |Zipped primitives with their IDs
primitiveIDs :: [(SutPrimitive, SutTypeID)]
primitiveIDs = zip primitives [0..]

-- |Eventual TypeIDs for the type graph
primitiveID :: SutPrimitive -> SutTypeID
primitiveID p = fromJust $ lookup p primitiveIDs

-- |Graph for primitives generalization
primitiveEdges :: [(SutPrimitive, SutTypeID, [SutTypeID])]
primitiveEdges =
  [ (SutTypeError, primitiveID SutTypeError, []                 )
  , (SutTypeVoid,  primitiveID SutTypeVoid,  [primitiveID SutLight]  )
  , (SutLight,     primitiveID SutLight,     [primitiveID SutLetter] )
  , (SutLetter,    primitiveID SutLetter,    [primitiveID SutBag]    )
  , (SutBag,       primitiveID SutBag,       [primitiveID SutWallet] )
  , (SutWallet,    primitiveID SutWallet,    []                 )
  , (SutPhrase,    primitiveID SutPhrase,    []                 )
  ]


primitiveG :: Graph
(primitiveG, _, _) = graphFromEdges primitiveEdges


-- |Given two types, this is the most general one (LCA)
-- Right now LCA is just always one of them
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

-- |Convert primitive to boolean type
toTypeLight :: SutPrimitive -> SutPrimitive
toTypeLight SutTypeVoid = SutLight
toTypeLight SutLight    = SutLight
toTypeLight SutLetter   = SutLight
toTypeLight SutBag      = SutLight
toTypeLight _           = SutTypeError

-- |Convert primitive to the most specific numerical type available
toTypeBag :: SutPrimitive -> SutPrimitive
toTypeBag SutTypeVoid = SutBag
toTypeBag SutLight    = SutBag
toTypeBag SutLetter   = SutBag
toTypeBag SutBag      = SutBag
toTypeBag SutWallet   = SutBag
toTypeBag _           = SutTypeError

-- |Convert primitive to the most specific numerical type available
toTypeNum :: SutPrimitive -> SutPrimitive
toTypeNum SutTypeVoid = SutBag
toTypeNum SutLight    = SutBag
toTypeNum SutLetter   = SutBag
toTypeNum SutBag      = SutBag
toTypeNum SutWallet   = SutWallet
toTypeNum _           = SutTypeError

-- |Convert primitive to the general float type
toTypeWallet :: SutPrimitive -> SutPrimitive
toTypeWallet SutTypeVoid = SutWallet
toTypeWallet SutLight    = SutWallet
toTypeWallet SutLetter   = SutWallet
toTypeWallet SutBag      = SutWallet
toTypeWallet SutWallet   = SutWallet
toTypeWallet _           = SutTypeError

-- |Convert primitive to the most general string (printable) type
toTypePhrase :: SutPrimitive -> SutPrimitive
toTypePhrase SutTypeError = SutTypeError
toTypePhrase _            = SutPhrase

-- |Convert primitive to a sortable type or error
toTypeSortable :: SutPrimitive -> SutPrimitive
toTypeSortable SutTypeVoid = SutLetter
toTypeSortable SutLight    = SutLetter
toTypeSortable SutLetter   = SutLetter
toTypeSortable SutBag      = SutBag
toTypeSortable SutWallet   = SutWallet
toTypeSortable SutPhrase   = SutPhrase
toTypeSortable _           = SutTypeError

-- |Convert primitive to a equalable type or error
toTypeEq :: SutPrimitive -> SutPrimitive
toTypeEq SutTypeVoid  = SutTypeError
toTypeEq a            = a
