{-|
  Description : Provides 'ShowSut' instances for "Sutori.TAC"
-}
module Sutori.TAC.Logger() where

import qualified Data.Map as Map

import Sutori.Logger       (SutShow(showSut), SutLog(SutLogNode, SutLogLeave), fromLeave)
import Sutori.AST
import Sutori.TAC.TAC

instance SutShow TACTable where
  showSut (TACTable is ts fs ls) = let instrs   = SutLogNode "TAC Instructions (refs)" [SutLogLeave "--- No optimizations done, still sorted --"]
                                       triplets = SutLogNode "TAC Triplets" $ zipWith f [0..] (reverse ts)
                                       functs   = SutLogNode "Fun Labels" $ map showF $ Map.toAscList fs
                                       labels   = SutLogNode "Inc Labels" $ map showL $ Map.toAscList ls
                                    in SutLogNode "TAC Table" [instrs, triplets, functs, labels]
                                   where f i tr  = SutLogLeave $ show i ++ "\t: " ++ fromLeave (showSut tr)
                                         showL (l,i) = SutLogLeave $ "LABEL_" ++ show l ++ "\t: (( " ++ show i ++ " ))"
                                         showF (f,i) = SutLogLeave $ "FUN_" ++ f ++ "\t: (( " ++ show i ++ " ))"

instance SutShow TACAddress where
  showSut (TACOffset off)                 = fillTabLog $ "base[" ++ show off ++ "]"
  showSut (TACGlobal sid)                 = fillTabLog $ "&(" ++ sid ++ ")"
  showSut (TACID i)                       = fillTabLog $ "(( " ++ show i ++ " ))"
  showSut (TACLabel i)                    = fillTabLog $ "LABEL_" ++ show i
  showSut (TACFun f)                      = fillTabLog $ "FUN_" ++ f
  showSut (TACLit (SutString (a:b:c:_)))  = fillTabLog $ "Phrase(\"" ++ [a,b,c] ++"..\")"
  showSut (TACLit (SutString s))          = fillTabLog $ "Phrase(\"" ++ s ++"\")"
  showSut (TACLit l)                      = fillTabLog $ fromLeave (showSut l)

fillTabLog :: String -> SutLog
fillTabLog s = SutLogLeave $ s ++ replicate (8 - length s) ' '

instance SutShow TAC where
  showSut TAC{ tacType = tt, tac1 = t1, tac2 = t2 } =
    let t'  = fromLeave' tt
        t1' = fromMLeave t1
        t2' = fromMLeave t2
     in SutLogLeave $ "\t\t" ++ t' ++ "\t" ++ t1' ++ "\t  " ++ t2'
    where fromLeave' = fromLeave . showSut
          fromMLeave (Just l) = fromLeave $ showSut l
          fromMLeave Nothing  = "_\t"
  showSut (Label l)    = SutLogLeave $ "LABEL_" ++ show l ++ ":"
  showSut (FunLabel l) = SutLogLeave $ "FUN_" ++ l ++ ":"

instance SutShow TACType where
  showSut (Basic op)  = SutLogLeave $ "Basic (" ++ fromLeave (showSut op) ++ ")"
  showSut Copy        = SutLogLeave "Copy       "
  showSut Jump        = SutLogLeave "Jump       "
  showSut JumpUnless  = SutLogLeave "JumpUnless "
  showSut Addr        = SutLogLeave "Address    "
  showSut Pointed     = SutLogLeave "Pointed    "
  showSut Param       = SutLogLeave "Param      "
  showSut Call        = SutLogLeave "Call       "
  showSut Return      = SutLogLeave "Return     "
  showSut (SysCall s) = SutLogLeave $ "Sys (" ++ fromLeave (showSut s) ++ ")"

instance SutShow SutSys where
  showSut SysRead   = SutLogLeave "IO:Read"
  showSut SysPrint  = SutLogLeave "IO:Prnt"
  showSut SysAlloc  = SutLogLeave "ME:Allc"
  showSut SysFree   = SutLogLeave "ME:Free"
