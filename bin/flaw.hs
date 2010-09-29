{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  , handleLogI, inumTee, stderrLog
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Exception (finally)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import           Data.IterIO
import           Data.IterIO.Http
import qualified Data.ListLike as LL

type L = L.ByteString
type S = S8.ByteString

port :: Net.PortNumber
port = 8001

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]

urlencoded :: S
urlencoded = S8.pack "application/x-www-form-urlencoded"

multipart :: S
multipart = S8.pack "multipart/form-data"

formPage :: Maybe S -> Html
formPage encM = page t $ toHtml
  [ h1 << t
  , paragraph << maybe "It will be submitted with the browser's default Content-Type."
                       (("It will be submitted with Content-Type: " ++) . S8.unpack)
                       encM
  , form ! ([ action "/submit", method "POST" ]
            ++ maybe [] ((:[]) . enctype . S8.unpack) encM) <<
      [ textfield "data1", br
      , textfield "data2", br
      , afile "file1", br
      , afile "file2", br
      , submit "what" "Go"
      ]
  ]
 where
  t = "Please complete this form"

handleReq :: (MonadIO m) => Inum L [Html] m a
handleReq = mkInumM $ do
  req <- lift httpreqI
  case reqMethod req of
    "GET" ->
      case reqPathLst req of
        [] -> echo req
        (x:_) -> case x of
                  "slow" -> do liftIO $ threadDelay $ 5 * 1000 * 1000
                               echo req
                  "form" -> ok $ formPage Nothing
                  "form-urlencoded" -> ok $ formPage (Just urlencoded)
                  "form-multipart" -> ok $ formPage (Just multipart)
                  _ -> echo req
    "POST" -> echo req
    _ -> error $ "Unrecognized method"
 where
  echo req = do
    h <- lift $ request2Html req
    ok $ page "Request" h

ok :: (MonadIO m, ChunkData t) => Html -> InumM t [Html] m a ()
ok p = ifeed [p] >> return ()

request2Html :: (Monad m) => HttpReq -> Iter L m Html
request2Html req = do
  parms <- foldParms [] getPart
  return (header2Html req +++ parms2Html parms)
 where
  parms2Html parts = ulist << (map ((li <<) . parm2Html) parts)
  parm2Html (mp,front,backLen) = toHtml
    [ strong << (S8.unpack (mpName mp) ++ ": ")
    , thespan << L.unpack front
    , if backLen > 0
        then emphasize << ("... (" ++ show (fromIntegral (L.length front) + backLen) ++ " bytes)")
        else noHtml
    ]
  getPart parts mp = do
    front <- takeExactI 50
    backLen <- countI
    return ((mp,front,backLen):parts)
  foldParms = case reqContentType req of
                Nothing -> foldQuery req
                _ -> foldForm req

-- showContentType (typ,_) = toHtml $ S8.unpack typ

countI :: (Monad m, ChunkData t, LL.ListLike t e) =>
          Iter t m Int
countI = more 0
 where
  more n = do
    eof <- atEOFI
    if eof
      then return n
      else do buf <- dataI
              more (n + LL.length buf)

header2Html :: HttpReq -> Html
header2Html r = toHtml [ requestLine, headers, cookies ]
 where
  requestLine = paragraph <<
     [ toHtml $ S8.unpack (reqMethod r) ++ " "
     , strong <<
         (S8.unpack (reqHost r)
          ++ (maybe "" (\p -> ":" ++ show p) $ reqPort r)
          ++ S8.unpack (reqPath r)
          ++ (if S8.null q
                then ""
                else "?" ++ S8.unpack q))
     , toHtml $ " HTTP/" ++ show major ++ "." ++ show minor
     ]
  (major,minor) = reqVers r
  q = reqQuery r
  headers = defs2Html $ reqHeaders r
  cookies = defs2Html $ reqCookies r
  def2Html (h,v) = toHtml [ strong << (S8.unpack h ++ ": ")
                          , toHtml $ S8.unpack v ]
  defs2Html dd = if null dd
                 then noHtml
                 else ulist << (map ((li <<) . def2Html) dd)

html2L :: (Monad m) => Inum [Html] L m a
html2L = mkInum $ do
  h <- headLI
  return $ L.append (headersL xhtmlHeaders)
                    (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-type: text/html"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']

handleLogI :: (MonadIO m, ChunkData t, LL.ListLikeIO t e, Eq t, Enum e, Eq e) =>
              IO.Handle -> t -> Iter t m ()
handleLogI h prefix = forever $ do
  line <- lineI
  liftIO $ LL.hPutStr h prefix
  liftIO $ LL.hPutStrLn h line

inumTee :: (Monad m, ChunkData t) =>
           Iter t m () -> Inum t t m a
inumTee = mkInumAutoM . loop
    where
      loop iter = do
        buf <- lift dataI
        iter' <- lift $ inumPure buf iter
        _ <- ifeed buf
        loop iter'

stderrLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e, Eq t, Enum e, Eq e) =>
             t -> Inum t t m a
stderrLog prefix = inumTee $ handleLogI IO.stderr prefix

-- server prototype

data Connection = Connection { cxTid :: ThreadId }
type Connections = MVar (Map ThreadId Connection)
data Event = CxDone ThreadId
           | CxNote ThreadId String
type Events = Chan Event

main :: IO ()
main = Net.withSocketsDo $ do
  listener <- myListen port
  connections <- newMVar Map.empty
  events <- newChan
  _ <- forkIO $ forever $ acceptConnection listener connections events
  forever $ handleEvent connections events

acceptConnection :: Net.Socket -> Connections -> Events -> IO ()
acceptConnection listener connections events = do
  (s, addr) <- Net.accept listener
  c <- spawnConnection s addr events
  modifyMVar_ connections $ return . Map.insert (cxTid c) c

handleEvent :: Connections -> Events -> IO ()
handleEvent connections events = do
  event <- readChan events
  case event of
    CxNote tid msg ->
      warn $ show tid ++ ": " ++ msg
    CxDone tid -> do
      modifyMVar_ connections $ return . Map.delete tid
      warn $ show tid ++ " FINISHED"

spawnConnection :: Net.Socket -> Net.SockAddr -> Events -> IO Connection
spawnConnection s addr events = do
  tid <- forkIO $ do
            tid <- myThreadId
            handleConnection s `finally` writeChan events (CxDone tid)
  writeChan events $
    CxNote tid $ "Handling connection from " ++ show addr
  return $ Connection tid

handleConnection :: Net.Socket -> IO ()
handleConnection s = do
  h <- Net.socketToHandle s IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  enumHandle' h -- |. stderrLog "< "
     |$ handleReq .| html2L .| handleI h

warn :: String -> IO ()
warn msg = IO.hPutStrLn IO.stderr msg

