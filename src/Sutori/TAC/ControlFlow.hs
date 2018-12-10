{-|
  Description: Control Flow Graph builder
-}

module Sutori.TAC.ControlFlow where

import Data.Maybe (fromJust, isJust)
import Data.Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Sutori.TAC.TAC


type PreNode       = [Int]

type FlowNode      = Vector.Vector Int
type FlowNodeKey   = Int
type FlowNodeEdges = (FlowNode, FlowNodeKey, [FlowNodeKey])
type FlowControlGraph = (Graph, Vertex -> FlowNodeEdges, FlowNodeKey -> Maybe Vertex)


printGraph (graph, v2e, k2v) = let vs = vertices graph in mapM_ (print . v2e) vs

flowNodes :: TACTable -> FlowControlGraph -> [Vector.Vector TAC]
flowNodes
  t@TACTable
    { tacInstructions = reversedInstrs
    , tacTriplets     = reversedTriplets
    , tacFunctions    = funs
    , tacLabels       = labels }
  (graph, v2e, k2v)
    = let vs = vertices graph
       in map (int2tacV.(\(t,_,_) -> t).v2e) vs
      where int2tacV :: Vector.Vector Int -> Vector.Vector TAC
            int2tacV = Vector.map int2tac
            int2tac :: Int -> TAC
            int2tac  = (Vector.!) tv
            tv :: Vector.Vector TAC
            tv = Vector.fromList $ reverse reversedTriplets


flowGraph :: TACTable -> FlowControlGraph
flowGraph t@TACTable
  { tacInstructions = reversedInstrs
  , tacTriplets     = triplets
  , tacFunctions    = funs
  , tacLabels       = labels }
  = let adjs = prepareNodes instrNodes in graphFromEdges adjs
    where
      -- | For fast indexing of triplets
      tv :: Vector.Vector TAC
      tv = Vector.fromList $ reverse triplets

      -- | An address' TAC ID number (triplet number ID)
      addrID :: TACAddress -> Int
      addrID (TACID i)    = i
      addrID (TACLabel i) = fromJust $ Map.lookup i labels
      addrID (TACFun i)   = fromJust $ Map.lookup i funs
      addrID _            = error "Trying to get triplet address for non-tripleted address"


      -- | The flow nodes (blocks of instructions)
      --
      -- Each node is a list of instructions (references to the 'tv' 'Vector' elements)
      instrNodes :: [PreNode]
      instrNodes = init $ uncurry (:) $ foldr buildNodes ([], []) reversedInstrs
        where
          instrs :: [Int]
          instrs = reverse reversedInstrs

          -- | Folding function. Adds current instruction to current node OR to next node, if it is leader
          buildNodes :: Int -> (PreNode, [PreNode]) -> (PreNode, [PreNode])
          buildNodes i (node, nodes) = if Set.member i blockLeaders then ([i], reverse node:nodes) else (i:node, nodes)
            where
              -- | The "block leader" instructions
              --
              -- Made of the very first instruction, if any, plus
              -- those instructions which are targets of explicit jumps, plus
              -- those instructions which inmediately follow a jump
              blockLeaders :: Set.Set Int
              blockLeaders = firstLeader instrs `Set.union` jumpTargets instrs `Set.union` afterJumps instrs
                where
                  -- | Takes the first instruction, if any, as leader
                  firstLeader :: [Int] -> Set.Set Int
                  firstLeader [] = Set.empty
                  firstLeader (i:is) = Set.singleton i

                  -- | Takes "next after jump" instructions as leaders
                  afterJumps :: [Int] -> Set.Set Int
                  afterJumps []       = Set.empty
                  afterJumps [_]      = Set.empty
                  afterJumps (a:b:xs) = let ai = tv Vector.! a
                                            mb = if isJump ai then Just b else Nothing
                                         in maybe Set.empty Set.singleton mb `Set.union` afterJumps (b:xs)

                  -- | Takes explicit jump targets as leaders
                  jumpTargets :: [Int] -> Set.Set Int
                  jumpTargets = foldr
                      (\i -> let instr = tv Vector.! i
                                 target = jumpTarget instr
                              in Set.union (maybe Set.empty Set.singleton target))
                      Set.empty
                    where
                        -- | The target instruction, if an explicit jump
                        jumpTarget :: TAC -> Maybe Int
                        jumpTarget TAC{tacType = Jump,       tac1 = Just addr} = Just $ addrID addr
                        jumpTarget TAC{tacType = JumpUnless, tac2 = Just addr} = Just $ addrID addr
                        jumpTarget _ = Nothing

      -- | Transforms a list of 'PreNode's to a list of  "Data.Graph" adjacency lists
      --
      -- Each node is a list of instruction IDs. Each gets an edge to a target of their jump, and to the next block.
      prepareNodes :: [PreNode] -> [FlowNodeEdges]
      prepareNodes = prepareNodes'
        where
          -- | A flow control 'PreNode' ID (to be considered \'key\' in the 'Graph'.
          nodeID :: PreNode -> FlowNodeKey
          nodeID = head

          -- | Transforms a raw node (list of instruction IDs) to a "Data.Graph" adjacency list
          prepareNode :: PreNode -> FlowNodeEdges
          prepareNode node = let lastInstID = last node
                                 lastInst   = tv Vector.! lastInstID
                                 lastIsJump = isJump lastInst
                                 nodeAdj    = let mt = anyJumpTarget lastInst in [fromJust mt | isJust mt]
                             in (Vector.fromList node, nodeID node, nodeAdj)
            where
              -- | The target instruction, if is an explicit jump
              anyJumpTarget :: TAC -> Maybe Int
              anyJumpTarget TAC{tacType = Jump,       tac1 = Just addr} = Just $ addrID addr
              anyJumpTarget TAC{tacType = JumpUnless, tac2 = Just addr} = Just $ addrID addr
              anyJumpTarget _ = Nothing

          prepareNodes' :: [PreNode] -> [FlowNodeEdges]
          prepareNodes' []       = []
          prepareNodes' [a]      = [prepareNode a]
          prepareNodes' (a:b:xs) = let (nodeV, nodeKey, nodeAdj) = prepareNode a
                                   in (nodeV, nodeKey, nodeID b : nodeAdj) : prepareNodes' (b:xs)


-- | Is the instruction an implicit or explicit jump?
isJump :: TAC -> Bool
isJump TAC{tacType = Jump}       = True
isJump TAC{tacType = JumpUnless} = True
isJump TAC{tacType = Return}     = True
isJump TAC{tacType = Call}       = True
isJump _ = False
