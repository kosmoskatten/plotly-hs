{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HBar where

import Control.Monad.State
import Data.Text (Text)

newtype HBar a = HBar { extrState :: StateT Context IO a }
  deriving (Functor, Applicative, Monad, MonadState Context, MonadIO)

data Context = Context

runHBar :: String -> String -> HBar a -> IO a
runHBar _host _descr act = evalStateT (extrState act) emptyContext

commit :: HBar ()
commit = undefined

addItemWithCategory :: Text -> Text -> Float -> HBar ()
addItemWithCategory = undefined

emptyContext :: Context
emptyContext = Context
