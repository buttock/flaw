{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Char (ord)
import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.Parse
import           Data.LargeWord (Word128)
import           Data.List (find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Maybe (isJust, fromMaybe)
import           Network.Socket (SockAddr(..))
import           System.FilePath.Posix (joinPath, combine, normalise, isRelative)
import           System.IO
import           Text.JSON
import           Text.XHtml.Strict hiding (p)

import qualified OpenSSL as SSL
import qualified Network.Socket as Net

import HttpServer
import App
import App.Cfg (cfg)

type L = L.ByteString
-- type S = S.ByteString

encodeJSValue :: JSValue -> String
encodeJSValue = encode

main :: IO ()
main = do
  time <- getCurrentTime
  logChan <- newChan
  _ <- forkIO $ logger logChan stderr
  stateVar <- newMVar $ initSt { stateKey = fromIntegral $ fromEnum $ utctDayTime time
                               , stateLog = Just $ logChan }
  let runConn addr = runReqM $ ReqInfo { riAddr = addr
                                       , riReq = error "no riReq"
                                       , riStateVar = stateVar
                                       }
  Net.withSocketsDo $ SSL.withOpenSSL $ do
    server <- mkHttpServer (cfgPort cfg) Nothing
    inOtherThread $ runHttpServer server handleRequest runConn

logger :: Chan String -> Handle -> IO ()
logger chan h = forever $ do
  line <- readChan chan
  hPutStrLn h line

--
-- Game state
--

newtype GameId = GameId Integer
  deriving (Eq, Ord)

data Event = Event { eventTime :: UTCTime
                   , eventData :: EventData
                   }

instance JSON Event where
  showJSON e = JSArray [ showJSON $ eventTime e
                       , showJSON $ eventData e
                       ]
  readJSON s = do dat <- readJSON s
                  return $ Event { eventTime = error "no time"
                                 , eventData = dat
                                 }

instance JSON UTCTime where
  showJSON u = showJSON $ timeSeconds u
  readJSON = error "readJSON UTCTime: unimplemented"

timeSeconds :: UTCTime -> Integer
timeSeconds = round . utcTimeToPOSIXSeconds

data EventData = NoteEvent String 

instance JSON EventData where
  showJSON (NoteEvent s) = showJSON s
  readJSON s = NoteEvent <$> readJSON s

data Game = Game { gameId :: GameId
                 , gameStart :: UTCTime
                 , gameEvents :: [Event]
                 , gameNEvents :: Int
                 , gameEventsReady :: Chan ()
                 , gameLibUrl :: Maybe String
                 }

defaultGame :: Game
defaultGame = Game { gameId = error "uninitialized gameId"
                   , gameStart = error "uninitialized gameStart"
                   , gameEvents = []
                   , gameNEvents = 0
                   , gameEventsReady = error "uninitialized gameEventsReady"
                   , gameLibUrl = error "uninitialized gameLibUrl"
                   }

instance Show Game where
  show game = show $ gameStart game

gameEventsFrom :: Int -> Game -> [Event]
gameEventsFrom n' g = more (gameNEvents g) (gameEvents g) []
 where
  more n ee ee' = if n > n'
                    then more (n-1) (tail ee) (head ee : ee')
                    else ee'

--
-- System state
--

data St = St { stateGames :: Map GameId Game
             , stateNextGameId :: Integer
             , stateKey :: Word128
             , stateLog :: Maybe (Chan String)
             }

initSt :: St
initSt = St { stateGames = Map.empty
            , stateNextGameId = 0
            , stateKey = 42
            , stateLog = Nothing
            }

--
-- Request-processing environment
--

data ReqInfo = ReqInfo { riAddr :: SockAddr
                       , riReq :: HttpReq
                       , riStateVar :: MVar St
                       }

type ReqM = ReaderT ReqInfo IO

runReqM :: ReqInfo -> ReqM a -> IO a
runReqM ri m = runReaderT m ri

getSt :: ReqM St
getSt = do
  stateVar <- asks riStateVar
  liftIO $ readMVar stateVar

{-
getReq :: ReqM HttpReq
getReq = asks riReq
-}

getPeerAddr :: ReqM SockAddr
getPeerAddr = asks riAddr

modifySt :: (St -> IO (St, a)) -> ReqM a
modifySt f = do
  stateVar <- asks riStateVar
  liftIO $ modifyMVar stateVar f

warn :: String -> ReqM ()
warn msg = do
  chM <- stateLog <$> getSt
  case chM of
    Nothing -> return ()
    Just ch -> liftIO $ writeChan ch msg

--
-- Request-processing Iteratee
--

type ReqI a = Iter L ReqM a
type ReqIResp = ReqI (HttpResp ReqM)
type RouteFn = HttpReq -> ReqIResp

getStI :: ReqI St
getStI = lift $ getSt

--
-- Request handler
--

handleRequest :: HttpReq -> ReqI (HttpResp ReqM)
handleRequest req = do
  addr <- lift getPeerAddr
  resp <- fromMaybe (notFound "Not Found") $ runHttpRoute topRoute req
  lift $ warn $ showReqLine addr req resp
  return resp

topRoute :: HttpRoute ReqM
topRoute = 
  routeMap [("games", gamesRoute)
           ,("dealer", routeVar $ gameRoute' True)
           ,("pub", jsFileRoute "pub")
           ,("game", jsFileRoute "game")
           ]

jsFileRoute :: FilePath -> HttpRoute ReqM
jsFileRoute path = routeMethod "GET" $ routeFn $ \req ->
      case safeFilePath path $ map S.unpack (reqPathLst req) of
        Nothing -> badRequest "Bad file path"
        Just filePath -> okJSFile filePath

gamesRoute :: HttpRoute ReqM
gamesRoute =
  mconcat [ routeTop $ mconcat [ routeMethod "GET" $ routeFn showGames
                               , routeMethod "POST" $ routeFn startGame
                               ]
          , routeVar $ gameRoute' False
          ]

showGames :: RouteFn
showGames _ = do
  s <- getStI
  links <- mapM gameLink $ Map.elems (stateGames s)
  ok $ page "Games" $ thediv <<
    [ h2 << "Running Games"
    , if null links
        then toHtml "No games have been started yet."
        else ulist << map (li <<) links
    , hr
    , h2 << "New Game"
    , thediv << form ! [ action "/games", method "POST" ] <<
        [ paragraph << [ radio "gameLibUrl" (gamePath "Flaw.js"), toHtml " Flaw", br
                       , radio "gameLibUrl" (gamePath "Chat.js") ! [checked], toHtml " Chat", br
                       , radio "gameLibUrl" "other", toHtml " ", textfield "otherLibUrl" ! [size "30"]
                       ]
        , paragraph << submit "start" "Start"
        ]
    ]

startGame :: RouteFn
startGame req = do
  now <- liftIO getCurrentTime
  ch <- liftIO $ newChan
  controls <- getControls req
  let libUrl = findDict "gameLibUrl" controls
  let libUrl' = if libUrl == Just "other"
                  then findDict "otherLibUrl" controls
                  else libUrl
  game <- lift $ modifySt $ \state ->
    let i = stateNextGameId state
        gi = GameId i
        game = defaultGame { gameId = gi
                           , gameStart = now
                           , gameEventsReady = ch
                           , gameLibUrl = libUrl'
                           }
        games' = Map.insert gi game (stateGames state)
        state' = state { stateNextGameId = succ i
                       , stateGames = games'
                       }
    in return (state', game)
  url <- gameDealerUrl game
  link <- gameDealerLink game
  seeOther url $ page "Game created" $ toHtml link 

findDict :: String -> [(String, String)] -> Maybe String
findDict key = fmap snd . find ((== key) . fst) 

getGame :: String -> Bool -> ReqI (Maybe Game)
getGame ident isDealer = do
  s <- getStI
  let ii' :: Integer = fromMaybe 0 $ Base32.decode $ ident
      i' :: Word128 = fromIntegral ii'
      offset = if isDealer then 1 else 0
      i :: Integer = fromIntegral $ AES.decrypt (stateKey s + offset) i'
  let gi = GameId i
  return $ Map.lookup gi (stateGames s)

gameRoute' :: Bool -> HttpRoute ReqM
gameRoute' isDealer = routeReq $ \req ->
  let ident = S.unpack $ head $ reqPathParams req
  in gameRoute ident isDealer

gameRoute :: String -> Bool -> HttpRoute ReqM
gameRoute ident isDealer =
  mconcat [ routeTop $ routeFn $ showGame ident isDealer
          , routeMap [("events", mconcat [ routeMethod "GET" $ getEvents ident isDealer
                                         , routeMethod "POST" $ routeTop $ routeFn $ postEvents ident isDealer
                                         ])]
          ]

showGame :: String -> Bool -> RouteFn
showGame ident isDealer _ = do
  gM <- getGame ident isDealer
  case gM of
    Nothing -> notFound "no such game"
    Just g -> do
      url <- (if isDealer then gameDealerUrl else gameUrl) g
      ok $ gamePage g url isDealer

getEvents :: String -> Bool -> HttpRoute ReqM
getEvents ident isDealer = routeVar $ routeTop $ routeFn $ \req -> do
  gM <- getGame ident isDealer
  case gM of
    Nothing -> notFound "No such game"
    Just g -> let n = read $ S.unpack $ head $ reqPathParams req
              in getEventsFrom g n

getEventsFrom :: Game -> Int -> ReqIResp
getEventsFrom g n =
  if null events
    then waitEventsFrom g n
    else okJSON events
 where
  events = gameEventsFrom n g

waitEventsFrom :: Game -> Int -> ReqIResp
waitEventsFrom g n = do
  ready <- liftIO $ dupChan (gameEventsReady g)
  _ <- liftIO $ readChan ready
  s <- getStI
  case Map.lookup (gameId g) (stateGames s) of
    Nothing -> badRequest "Game no longer available"
    Just g' -> getEventsFrom g' n

postEvents :: String -> Bool -> RouteFn
postEvents ident isDealer _ = do
  gM <- getGame ident isDealer
  case gM of
    Nothing -> notFound "no such game"
    Just g -> do
      eventsS <- netstringI
      let events = either error id $ resultToEither $ decode $
                     U.toString eventsS
      let gi = gameId g
      gameM <- lift $ modifySt $ \s ->
        case Map.lookup gi (stateGames s) of
          Nothing -> return (s, Nothing)
          Just g' -> do
            now <- getCurrentTime
            let events' = map (\e -> e { eventTime = now }) events
                g'' = g' { gameEvents = events' ++ gameEvents g'
                         , gameNEvents = length events' + gameNEvents g'
                         }
                s' = s { stateGames = Map.insert gi g'' $ stateGames s }
            return $ (s', Just g'')
      case gameM of
        Nothing -> badRequest "Game no longer available"
        Just game -> do
          liftIO $ writeChan (gameEventsReady game) ()
          ok "OK"

--
-- Url
--

gameLink :: Game -> ReqI HotLink
gameLink g = do
  url <- gameUrl g
  return $ hotlink url $ toHtml $ show g

gameDealerLink :: Game -> ReqI HotLink
gameDealerLink g = do
  url <- gameDealerUrl g
  return $ hotlink url $ toHtml $ show g

gameUrl :: Game -> ReqI URL
gameUrl g = do
  s <- getStI
  let (GameId i) = gameId g
      i' :: Integer = fromIntegral $ AES.encrypt (stateKey s) $ fromIntegral i
  return $ "/games/" ++ Base32.encode i'

gameDealerUrl :: Game -> ReqI URL
gameDealerUrl g = do
  s <- getStI
  let (GameId i) = gameId g
      i' :: Integer = fromIntegral $ AES.encrypt (stateKey s + 1) $ fromIntegral i
  return $ "/dealer/" ++ Base32.encode i'

--
-- Html
--

gamePage :: Game -> URL -> Bool -> Html
gamePage g url isDealer =
  thehtml <<
     [ header <<
         [ meta ! [ httpequiv "Content-Type"
                  , content "text/html; charset=UTF-8"
                  ]
         , thetitle << (show g ++ if isDealer then " (Dealer)" else "")
         , jsLib $ pubPath "js.js"
         , jsLib $ pubPath "dom.js"
         , jsLib $ pubPath "ajax.js"
         , jsLib $ pubPath "json2-min.js"
         , jsLib $ pubPath "App.js"
         , jsLib $ pubPath "AppUtils.js"
         , maybe noHtml jsLib $ gameLibUrl g
         , js $ "App.setCfg("
                ++ (encodeJSValue $ makeObj
                      [("url", showJSON url)
                      ,("gameHomeId", showJSON "game")
                      ,("isDealer", showJSON isDealer)
                      ])
                ++ ");\n"
         ]
     , body ! [ onload "App.onload();" ] <<
         [ h1 << show g
         , thediv ! [ identifier "game" ] << "Hello."
         ]
     ]

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]

jsLib :: FilePath -> Html
jsLib path = tag "script" noHtml ! [thetype "text/javascript", src path]

js :: String -> Html
js code = tag "script" (primHtml code) ! [thetype "text/javascript"]

-- cssLib path = itag "link" ! [href path, rel "stylesheet", thetype "text/css"]

pubPath :: FilePath -> FilePath
pubPath = combine "/pub/"

gamePath :: FilePath -> FilePath
gamePath = combine "/game/"

onload :: String -> HtmlAttr
onload = strAttr "onload"

-- onsubmit :: String -> HtmlAttr
-- onsubmit = strAttr "onsubmit"

--
-- Utils
--

showReqLine :: (Monad m) => SockAddr -> HttpReq -> HttpResp m -> String
showReqLine addr req resp =
  show addr
  ++ " " ++ S.unpack (reqMethod req)
  ++ " " ++ S.unpack (reqPath req)
  ++ " -> " ++ showStatus (respStatus resp)

showStatus :: HttpStatus -> String
showStatus (HttpStatus code desc) = show code ++ " " ++ S.unpack desc

htmlResp :: (HTML h) => HttpStatus -> h -> HttpResp ReqM
htmlResp stat h = mkHtmlResp stat (U.fromString $ renderHtml $ toHtml h)
 
ok :: (HTML h) => h -> ReqI (HttpResp ReqM)
ok h = return $ htmlResp stat200 h

seeOther :: URL -> msg -> ReqI (HttpResp ReqM)
seeOther url _ = return $ resp303 url

badRequest :: (HTML h) => h -> ReqI (HttpResp ReqM)
badRequest h = return $ htmlResp stat400 h

notFound :: (HTML h) => h -> ReqI (HttpResp ReqM)
notFound h = return $ htmlResp stat404 h

okJSFile :: FilePath -> ReqI (HttpResp ReqM)
okJSFile path = return $ mkOnumResp stat200 "text/javascript" $ enumFile' path

okJSON :: (JSON a) => a -> ReqI (HttpResp ReqM)
okJSON x = return $ mkContentLenResp stat200 "text/json" contents
  where contents = U.fromString $ encodeJSValue $ showJSON $ x

safeFilePath :: FilePath -> [String] -> Maybe FilePath
safeFilePath basePath pp =
  if isJust $ find ((== '.') . head) $ filter (not . null) pp
  then Nothing
  else let fp = normalise $ combine basePath (joinPath pp)
       in if isRelative fp
          then Just fp
          else Nothing

netstringI :: (Monad m) => Iter L m L
netstringI = do
  numS <- whileI (/= (fromIntegral $ ord ':'))
  let num = read $ L.unpack numS -- XX: catch error
  _ <- char ':'
  buf <- takeExactI num
  let len = fromIntegral $ L.length buf
  when (len /= num) $
    expectedI ("data of length " ++ show len ++ ", followed by EOF")
              ("data of length " ++ show num)
  _ <- char ','
  return buf

inOtherThread :: IO () -> IO ()
inOtherThread m = do
  sem <- newQSem 0
  _ <- forkIO $ m `finally` signalQSem sem
  waitQSem sem

getControls :: (Monad m) => HttpReq -> Iter L m [(String, String)]
getControls req =
  let docontrol acc field = do
        val <- pureI
        return $ (S.unpack $ ffName field, L.unpack val) : acc
  in foldForm req docontrol []

