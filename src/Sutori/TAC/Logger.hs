{-|
  Description : Provides 'ShowSut' instances for "Sutori.TAC"
-}
module Sutori.TAC.Logger() where

import Sutori.Logger       (SutShow(showSut), SutLog(SutLogNode, SutLogLeave), fromLeave)

import Sutori.TAC.TAC

instance SutShow TACTable where
  showSut (TACTable is ts) = let instrs   = SutLogNode "TAC Instructions (refs)" [SutLogLeave "--- No optimizations done, still sorted --"]
                                 triplets = SutLogNode "TAC Triplets" $ zipWith f [0..] (reverse ts)
                              in SutLogNode "TAC Table" [instrs, triplets]
                             where f i tr = SutLogLeave $ show i ++ "\t: " ++ fromLeave (showSut tr)

instance SutShow TACAddress where
  showSut (TACName (sid, sc)) = SutLogLeave $ "Symbol:" ++ sid ++ " (in scope " ++ show sc ++ ")"
  showSut (TACID i)           = SutLogLeave $ "i_" ++ show i
  showSut (TACLit l)          = SutLogLeave $ fromLeave (showSut l)

instance SutShow TAC where
  showSut TAC{ tacType = tt, tac1 = t1, tac2 = t2 } =
    let t'  = fromLeave' tt
        t1' = fromMLeave t1
        t2' = fromMLeave t2
     in SutLogLeave $ t' ++ "\t" ++ t1' ++ "\t" ++ t2'
    where fromLeave' = fromLeave . showSut
          fromMLeave (Just l) = fromLeave $ showSut l
          fromMLeave Nothing  = "_"

instance SutShow TACType where
  showSut (Basic op)  = SutLogLeave $ "Basic (" ++ fromLeave (showSut op) ++ ")"
  showSut Copy        = SutLogLeave "Copy"
  showSut Jump        = SutLogLeave "Jump"
  showSut JumpUnless  = SutLogLeave "JumpUnless"
  showSut Array       = SutLogLeave "Array"
  showSut Pointed     = SutLogLeave "Pointed"
  showSut Param       = SutLogLeave "Param"
  showSut Call        = SutLogLeave "Call"
  showSut Return      = SutLogLeave "Return"
  showSut SysCall     = SutLogLeave "SysCall"
