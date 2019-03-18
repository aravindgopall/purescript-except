module Except.Runner where


import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans (StateT)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Except.Dsl (ExceptFlow, FlowE(..))
import Prelude (unit)
import Unsafe.Coerce (unsafeCoerce)


type Runner st e a = StateT st (ExceptT e Aff) a

run :: forall st a. ExceptFlow st a -> Runner st Error a
run flow = foldFree (\f -> interpret f) flow

interpret :: forall st a. FlowE st a -> Runner st Error a
interpret (Command cmd options) = unsafeCoerce unit
interpret (When str maybeResp) = unsafeCoerce unit
interpret (Respond resp) = unsafeCoerce unit
interpret (End f) = unsafeCoerce unit
