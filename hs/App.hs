module App ( Cfg(..)
           , defaultCfg
           ) where

data Cfg = Cfg { cfgPort :: Int }

defaultCfg = Cfg { cfgPort = 8800 }

