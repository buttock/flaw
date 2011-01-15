{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpServer (mkHttpServer, runHttpServer) where

import Prelude hiding (catch, head, id, div)
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.IO

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.SSL

type L = L.ByteString

data HttpServer = HttpServer {
      hsListenSock :: !Net.Socket
    , hsSslCtx :: !(Maybe SSL.SSLContext)
    , hsLog :: !(Maybe Handle)
    }

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

httpAccept :: HttpServer -> IO (Iter L IO (), Onum L IO a)
httpAccept hs = do
  (s, addr) <- Net.accept $ hsListenSock hs
  hPutStrLn stderr (show addr)
  maybe (mkInsecure s) (mkSecure s) (hsSslCtx hs)
  {-
  (iter, enum) <- maybe (mkInsecure s) (mkSecure s) (hsSslCtx hs)
  return $ maybe (iter, enum) |. inumhLog undefined)
                 (\h -> (inumhLog h .| iter, enum |. inumhLog h))
                 (hsLog hs)
  -}
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr (show e)
                       Net.sClose s
                       return (nullI, return)

mkHttpServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkHttpServer port mctx = do
  sock <- myListen port
  {-
  h <- openBinaryFile "http.log" WriteMode
  hSetBuffering h NoBuffering
  -}
  return $ HttpServer sock mctx Nothing

runHttpServer :: HttpServer -> HttpRoute IO -> IO ()
runHttpServer srv route = loop
  where
    loop = do
      (iter, enum) <- httpAccept srv
      _ <- forkIO $ enum |$ inumHttpServer (ioHttpServer route) .| iter
      loop

