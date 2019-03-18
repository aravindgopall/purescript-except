module Except.Runner where


import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.State.Trans (StateT)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Except.Dsl (ExceptFlow)
import Prelude (unit)
import Unsafe.Coerce (unsafeCoerce)


type Runner st e a = StateT st (ExceptT e Aff) a

run :: forall st a. ExceptFlow st a -> Runner st Error a
run flow = unsafeCoerce unit
