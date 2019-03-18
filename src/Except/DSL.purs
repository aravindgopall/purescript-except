module Except.Dsl where

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)
import Prelude (Unit, ($), (<<<))



data FlowE st a =  Command String (Array String)
               | When String (Maybe String)
               | Respond String


type ExceptFlow st a = Free (FlowE st) a


command :: forall st. String -> Array String -> ExceptFlow st Unit 
command cmd options = liftF $ Command cmd options

when :: forall st. String -> Maybe String -> ExceptFlow st Unit
when str = liftF <<< When str

respond :: forall st. String -> ExceptFlow st Unit
respond = liftF <<< Respond

