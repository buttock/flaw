module App ( Cfg(..)
           , defaultCfg
           ) where

import qualified Network.Socket as Net

data Cfg = Cfg { cfgPort :: Net.PortNumber }

defaultCfg :: Cfg
defaultCfg = Cfg { cfgPort = 8000 }

