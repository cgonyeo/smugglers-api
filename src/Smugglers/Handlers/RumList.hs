module Smugglers.Handlers.RumList where

import Servant
import Control.Monad.Trans.Except

import Smugglers.Data
import qualified Smugglers.Upstream.GetRums as R

getRumsForUser :: User -> ExceptT ServantErr IO [Rum]
getRumsForUser = R.getRumsForUser
