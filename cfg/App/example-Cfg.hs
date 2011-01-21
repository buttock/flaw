module App.Cfg (cfg) where

import App

cfg :: Cfg
cfg = defaultCfg { cfgPort = 8008 }

