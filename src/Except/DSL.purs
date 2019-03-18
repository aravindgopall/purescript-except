module Except.Dsl where

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Prelude (Unit, identity, ($))



data FlowE st a =  Command String (Array String) (Unit -> a)
               | When String (Maybe String) (Unit -> a)
               | Respond String (Unit -> a)
               | End (String -> Aff Unit) (Unit -> a)


type ExceptFlow st a = Free (FlowE st) a


command :: forall st. String -> Array String -> ExceptFlow st Unit 
command cmd options = liftF $ Command cmd options identity

when :: forall st. String -> Maybe String -> ExceptFlow st Unit
when str maybeResp = liftF $ When str maybeResp identity

respond :: forall st. String -> ExceptFlow st Unit
respond resp = liftF $ Respond resp identity

end :: forall st. (String -> Aff Unit) -> ExceptFlow st Unit
end f = liftF $ End f identity

