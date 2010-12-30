{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
import           Control.Monad (when)
import           Control.Monad.Trans
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
import           Data.Time.Clock
import           Data.Maybe (isJust, fromMaybe)
import           System.FilePath.Posix (joinPath, combine, normalise, isRelative)
import qualified System.IO as IO
import           Text.JSON
import           Text.XHtml.Strict hiding (p)

import Server

type L = L.ByteString
type S = S.ByteString

encodeJSValue :: JSValue -> String
encodeJSValue = encode

main :: IO ()
main = do
  time <- getCurrentTime
  stateM <- newMVar $ initState { stateKey = fromIntegral $ fromEnum $ utctDayTime time }
  let route = routeReqM stateM systemRoute
  server 8000 route

---
HttpRoute  
  runHttpRoute :: !(HttpReq -> Maybe (Iter ByteString m (HttpResp m)))
---

routeReqM :: MVar State -> HttpRoute ReqM -> HttpRoute IO
routeReqM stateM route =
  HttpRoute $
    fmap (adaptReqMI stateM (adaptResp $ runReqM stateM)) .
      runHttpRoute route

runReqM :: MVar State -> ReqM a -> IO a
runReqM stateM m = modifyMVar stateM $ fmap swap . runStateT m
{-
  modifyMVar stateM $ \state -> do
    (x, state') <- runStateT m state
    return (state', x)
-}

runReqMI :: State -> (a -> b) -> Iter t ReqM a -> Iter t IO b
runReqMI s adaptResult iter = adaptIter adaptResult adaptComputation iter
 where
  adaptComputation m = do
    iter' <- lift (runReqM m s)
    runStateTI iter' s'


--
-- Game state
--

newtype GameId = GameId Integer
  deriving (Eq, Ord)

data Event = Event { eventTime :: UTCTime
                   , eventData :: EventData
                   }

instance JSON Event where
  showJSON e = showJSON $ eventData e
  readJSON s = do dat <- readJSON s
                  return $ Event { eventTime = error "no time"
                                 , eventData = dat
                                 }

data EventData = NoteEvent String 

instance JSON EventData where
  showJSON (NoteEvent s) = showJSON s
  readJSON s = NoteEvent <$> readJSON s

data Game = Game { gameId :: GameId
                 , gameStart :: UTCTime
                 , gameEvents :: [Event]
                 , gameNEvents :: Int
                 , gameEventsReady :: Chan ()
                 }

defaultGame :: Game
defaultGame = Game { gameId = error "uninitialized gameId"
                   , gameStart = error "uninitialized gameStart"
                   , gameEvents = []
                   , gameNEvents = 0
                   , gameEventsReady = error "uninitialized gameEventsReady"
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

data State = State { stateGames :: Map GameId Game
                   , stateNextGameId :: Integer
                   , stateKey :: Word128
                   }

initState :: State
initState = State { stateGames = Map.empty
                  , stateNextGameId = 0
                  , stateKey = 42
                  }

--
-- Request-processing environment
--

type ReqM = StateT State IO

type RouteFn m = HttpReq -> Iter ByteString m (HttpResp  m)

--
-- Request handler
--

systemRoute :: HttpRoute ReqM
systemRoute = 
  routeMap [("games", gamesRoute)
           -- ,("dealer", dealerRoute)
           -- ,("pub", pubRoute)
           ]

gamesRoute :: HttpRoute ReqM
gamesRoute =
  mconcat [ routeTop $ mconcat [ routeMethod "GET" $ routeFn showGames
                               -- , routeMethod "POST" $ routeFn startGame
                               ]
          , routeVar $ gameRoute
          ]

showGames :: RouteFn ReqM
showGames = do
  ok $ page "Games" $ thediv <<
    [ h2 << "Games"
    , ulist << map ((li <<) . gameLink s) (Map.elems $ stateGames s)
    , thediv << form ! [ action "/games", method "POST" ] <<
        [ submit "start" "Start Game" ]
    ]

gameLink :: Game -> ReqM HotLink
gameLink g = hotlink (gameUrl g) $ toHtml $ show g

gameUrl :: Game -> ReqM URL
gameUrl g = getState >>= \s ->
  let (GameId i) = gameId g
      i' :: Integer = fromIntegral $ AES.encrypt (stateKey s) $ fromIntegral i
  return "/games/" ++ Base32.encode i'

 
{-
gameRoute g path isDealer =
    case path of
      [] -> showGame g isDealer
      (x:restPath) ->
        case S.unpack x of
          "events" -> 
            case S.unpack $ reqMethod req of
              "GET" -> getEvents g restPath
              "POST" -> postEvents g
              _ -> badRequest "Inappropriate method"
          _ -> notFound'


      (is:restPath) -> do
        s <- getState
        let ii' :: Integer = fromMaybe 0 $ Base32.decode $ S.unpack is
            i' :: Word128 = fromIntegral ii'
            i :: Integer = fromIntegral $ AES.decrypt (stateKey s) i'
        let gi = GameId i
        case Map.lookup gi (stateGames s) of
          Nothing -> notFound "No such game"
          Just game -> gameRequest game restPath False

handleRequest :: MVar State -> IO.Handle -> IO ()
handleRequest stateM h = enumHandle' h |$ handleRequestI stateM h

handleRequestI :: (MonadIO m) => MVar State -> IO.Handle -> Iter L m ()
handleRequestI stateM h = do
  req <- httpreqI
  handleRequestI' stateM h req

handleRequestI' :: (MonadIO m) => MVar State -> IO.Handle -> HttpReq -> Iter L m ()
handleRequestI' stateM h req = topRequest (reqPathLst req)
 where
  topRequest path =
    case path of
      (x:restPath) ->
        case S.unpack x of
          "games" -> gamesRequest restPath
          "dealer" -> dealerRequest restPath
          "pub" -> pubRequest restPath
          _ -> notFound'
      _ -> notFound'

  pubRequest path =
    case S.unpack $ reqMethod req of 
      "GET" ->
        -- XX: an exception here should produce "500 Internal Server Error",
        -- or "404 Not Found" if the file does not exist
        case safeFilePath ("pub" : map S.unpack path) of
          Nothing -> badRequest "Bad file path"
          Just filePath -> do
            warn $ "GET " ++ filePath
            ok $ FileMsg mimetype'javascript filePath
      _ -> badRequest "Method not allowed"

  dealerRequest path =
    case path of
      (is:restPath) -> do
        s <- getState
        let ii' :: Integer = fromMaybe 0 $ Base32.decode $ S.unpack is
            i' :: Word128 = fromIntegral ii'
            i :: Integer = fromIntegral $ AES.decrypt (stateKey s + 1) i'
        let gi = GameId i
        case Map.lookup gi (stateGames s) of
          Nothing -> notFound "No such game"
          Just game -> gameRequest game restPath True
      _ -> notFound'

  gamesRequest path =
    case path of
      [] -> case S.unpack $ reqMethod req of 
              "GET" -> showGames
              "POST" -> startGame
              _ -> badRequest "Method not allowed."
      (is:restPath) -> do
        s <- getState
        let ii' :: Integer = fromMaybe 0 $ Base32.decode $ S.unpack is
            i' :: Word128 = fromIntegral ii'
            i :: Integer = fromIntegral $ AES.decrypt (stateKey s) i'
        let gi = GameId i
        case Map.lookup gi (stateGames s) of
          Nothing -> notFound "No such game"
          Just game -> gameRequest game restPath False

  startGame = do
    now <- io getCurrentTime
    ch <- io $ newChan
    (s, game) <- io $ modifyMVar stateM $ \state ->
      let i = stateNextGameId state
          gi = GameId i
          game = defaultGame { gameId = gi, gameStart = now, gameEventsReady = ch }
          games' = Map.insert gi game (stateGames state)
          state' = state { stateNextGameId = succ i
                         , stateGames = games'
                         }
      in return (state', (state', game))
    seeOther (gameDealerUrl s game) $ page "Game created" $ toHtml $ gameDealerLink s game

  gameRequest g path isDealer =
    case path of
      [] -> showGame g isDealer
      (x:restPath) ->
        case S.unpack x of
          "events" -> 
            case S.unpack $ reqMethod req of
              "GET" -> getEvents g restPath
              "POST" -> postEvents g
              _ -> badRequest "Inappropriate method"
          _ -> notFound'

  showGame g isDealer = do
    s <- getState
    let url = (if isDealer then gameDealerUrl else gameUrl) s g
    ok $ gamePage g url isDealer

  showGames = do
    s <- getState
    ok $ page "Games" $ thediv <<
      [ h2 << "Games"
      , ulist << map ((li <<) . gameLink s) (Map.elems $ stateGames s)
      , thediv << form ! [ action "/games", method "POST" ] <<
          [ submit "start" "Start Game" ]
      ]

  getEvents g path =
    case path of
      [fromS] -> getEventsFrom g $ read $ S.unpack fromS
      _ -> notFound'

  getEventsFrom g n =
    let events = gameEventsFrom n g
    in if null events
      then do
        ready <- io $ dupChan (gameEventsReady g)
        _ <- io $ readChan ready
        getEventsFrom' g n  
      else
        sendEvents events

  getEventsFrom' g n = do
    let gi = gameId g
    s <- getState
    case Map.lookup gi (stateGames s) of
      Nothing -> badRequest "Game no longer available"
      Just g' -> getEventsFrom g' n

  sendEvents events =
    ok $ JSONMessage $ showJSON events

  postEvents g = do
    -- io $ threadDelay $ 2 * 1000 * 1000
    eventsS <- netstringI
    let events = either error id $ resultToEither $ decode $
                   U.toString eventsS
    let gi = gameId g
    gameM <- io $ modifyMVar stateM $ \s ->
      case Map.lookup gi (stateGames s) of
        Nothing -> return (s, Nothing)
        Just g' -> do
          -- now <- getCurrentTime
          let g'' = g' { gameEvents = events ++ gameEvents g'
                       , gameNEvents = length events + gameNEvents g'
                       }
              s' = s { stateGames = Map.insert gi g'' $ stateGames s }
          return $ (s', Just g'')
    case gameM of
      Nothing -> badRequest "Game no longer available"
      Just game -> do
        io $ writeChan (gameEventsReady game) ()
        ok "OK"

  gameLink :: State -> Game -> HotLink
  gameLink s g = hotlink (gameUrl s g) $ toHtml $ show g

  gameUrl :: State -> Game -> URL
  gameUrl s g = "/games/" ++ Base32.encode i'
   where
    (GameId i) = gameId g
    i' :: Integer = fromIntegral $ AES.encrypt (stateKey s) $ fromIntegral i

  gameDealerLink :: State -> Game -> HotLink
  gameDealerLink s g = hotlink (gameDealerUrl s g) $ toHtml $ show g

  gameDealerUrl :: State -> Game -> URL
  gameDealerUrl s g = "/dealer/" ++ Base32.encode i'
   where
    (GameId i) = gameId g
    i' :: Integer = fromIntegral $ AES.encrypt (stateKey s + 1) $ fromIntegral i

  messageI :: (Message msg, MonadIO m) => S -> [S] -> msg -> Iter L m ()
  messageI status headers msg =
    inumMsg status headers msg .| handleI h
  
  ok :: (Message msg, MonadIO m) => msg -> Iter L m ()
  ok msg = messageI statusOK [] msg

  seeOther :: (Message msg, MonadIO m) => URL -> msg -> Iter L m ()
  seeOther url = messageI statusSeeOther [S.pack $ "Location: " ++ url]

  notFound :: (Message msg, MonadIO m) => msg -> Iter L m ()
  notFound = messageI statusNotFound []

  notFound' = notFound "Not Found"

  badRequest :: (Message msg, MonadIO m) => msg -> Iter L m ()
  badRequest = messageI statusBadRequest []

  getState = io $ readMVar stateM

  warn s = io $ IO.hPutStrLn IO.stderr s

io :: (MonadIO m) => IO a -> m a
io x = liftIO x
-}


--
-- JSON
--

data JSONMessage = JSONMessage JSValue

instance Message JSONMessage where
  msgContentType _ = mimetype'json
  msgBytes (JSONMessage json) = U.fromString $ encodeJSValue json


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
         , jsLib $ pubPath "flaw.js"
         , js $ "configGame("
                ++ (encodeJSValue $ makeObj
                      [("url", showJSON url)
                      ,("gameHomeId", showJSON "game")
                      ,("isDealer", showJSON isDealer)
                      ])
                ++ ");"
         ]
     , body ! [ onload "onloadHandler();" ] <<
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

onload :: String -> HtmlAttr
onload = strAttr "onload"

-- onsubmit :: String -> HtmlAttr
-- onsubmit = strAttr "onsubmit"

--
-- Utils
--

safeFilePath :: [String] -> Maybe FilePath
safeFilePath pp =
  if isJust $ find ((== '.') . head) $ filter (not . null) pp
  then Nothing
  else let fp = normalise $ joinPath pp
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

