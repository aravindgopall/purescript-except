module Except.Runner where


import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (foldFree)
import Control.Monad.State (modify_)
import Control.Monad.State.Trans (StateT)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Except.Dsl (ExceptFlow, FlowE(..))
import Foreign.Object (Object, insert, pop)
import Prelude (pure, unit, (<<<), (>>=))
import Unsafe.Coerce (unsafeCoerce)


type Runner st e a = StateT (Object String) (ExceptT e Aff) a

run :: forall st a. ExceptFlow st a -> Runner st Error a
run flow = foldFree (\f -> interpret f) flow

interpret :: forall st a. FlowE st a -> Runner st Error a
interpret (Command cmd options next) = unsafeCoerce unit
interpret (When str (Just resp) next) = modify_ (\st -> insert str resp st) >>= (pure <<< next) 
interpret (When str Nothing next) = modify_ (\st -> insert "lastwhen" str st) >>= (pure <<< next)
interpret (Respond resp next) = modify_ (handler resp) >>= (pure <<< next) 
interpret (End f next) = unsafeCoerce unit



handler :: String -> Object String -> Object String
handler resp st = do
    case pop "lastwhen" st of
         Just (Tuple key map) -> insert key resp map
         Nothing -> st
