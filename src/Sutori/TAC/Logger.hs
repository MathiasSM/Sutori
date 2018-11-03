{-|
  Description : Provides 'ShowSut' instances for "Sutori.TAC"
-}
module Sutori.TAC.Logger() where

import Sutori.Logger       (SutShow(showSut), SutLog(SutLogLeave, SutLogNode), fromLeave)

import Sutori.TAC.TAC

instance SutShow TACAddress where
  showSut (TACName sid) = SutLogLeave $ "Name:" ++ sid
  showSut (TACLit lit)  = SutLogLeave $ "Lit:" ++ fromLeave (showSut lit)
  showSut (TACTemp i)   = SutLogLeave $ "t_" ++ show i
  showSut (TACID i)     = SutLogLeave $ "i_" ++ show i

instance SutShow TAC where
  showSut TAC{ tacType = tt, tac1 = t1, tac2 = t2 } =
    let t'  = fromLeave' tt
        t1' = fromMLeave t1
        t2' = fromMLeave t2
     in SutLogLeave $ t' ++ "\t" ++ t1' ++ "\t" ++ t2'
    where fromLeave' = fromLeave . showSut
          fromMLeave (Just l) = fromLeave $ showSut l
          fromMLeave Nothing  = "\t"

instance SutShow TACType where
  showSut (Basic op)  = SutLogLeave $ "Basic (" ++ fromLeave (showSut op) ++ ")"
  showSut Copy        = SutLogLeave "Copy"
  showSut Jump        = SutLogLeave "Jump"
  showSut JumpIf      = SutLogLeave "JumpIf"
  showSut FromArray   = SutLogLeave "FromArray"
  showSut ToArray     = SutLogLeave "ToArray"
  showSut FromPointed = SutLogLeave "FromPointed"
  showSut ToPointed   = SutLogLeave "ToPointed"
  showSut Param       = SutLogLeave "Param"
  showSut Call        = SutLogLeave "Call"
  showSut Return      = SutLogLeave "Return"
