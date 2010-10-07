{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IterIO
import           Data.IterIO.Http
import           Data.LargeWord (Word128)
import           Data.List (find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Maybe (isJust, fromMaybe)
import           System.FilePath.Posix (joinPath, normalise, isRelative)
import qualified System.IO as IO
import           Text.JSON
import           Text.XHtml.Strict hiding (p)

import Server
import Message

type L = L.ByteString
type S = S.ByteString

main :: IO ()
main = do
  time <- getCurrentTime
  let state = initState { stateKey = fromIntegral $ fromEnum $ utctDayTime time }
  stateM <- newMVar state
  server 8000 (handleRequest stateM)

--
-- Game state
--

newtype GameId = GameId Integer
  deriving (Eq, Ord)

data Event = Event { eventTime :: UTCTime }

instance Show Event where
  show event = show $ eventTime event

instance JSON Event where
  showJSON e = showJSON (show e)

data Game = Game { gameId :: GameId
                 , gameStart :: UTCTime
                 , gameEvents :: [Event]
                 , gameNEvents :: Int
                 , gameEventsReady :: QSem
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
-- Request handler
--

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
    q <- io $ newQSem 0
    (s, game) <- io $ modifyMVar stateM $ \state ->
      let i = stateNextGameId state
          gi = GameId i
          game = defaultGame { gameId = gi, gameStart = now, gameEventsReady = q }
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
    ok $ gamePage g url

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
        io $ waitQSem (gameEventsReady g)
        getEventsFrom' g n  
      else
        sendEvents events

  getEventsFrom' g n = do
    let gi = gameId g
    s <- getState
    case Map.lookup gi (stateGames s) of
      Nothing -> badRequest "Game no longer available"
      Just g' -> getEventsFrom g' n

  postEvents g = do
    let gi = gameId g
    gameM <- io $ modifyMVar stateM $ \s ->
      case Map.lookup gi (stateGames s) of
        Nothing -> return (s, Nothing)
        Just g' -> do
          now <- getCurrentTime
          let e = Event now
              g'' = g' { gameEvents = e : gameEvents g'
                       , gameNEvents = succ $ gameNEvents g'
                       }
              s' = s { stateGames = Map.insert gi g'' $ stateGames s }
          return $ (s', Just g'')
    case gameM of
      Nothing -> badRequest "Game no longer available"
      Just game -> do
        io $ signalQSem (gameEventsReady game)
        ok "OK"

  sendEvents events =
    ok $ JSONMessage $ showJSON events

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


--
-- JSON
--

data JSONMessage = JSONMessage JSValue

instance Message JSONMessage where
  msgContentType _ = mimetype'json
  msgBytes (JSONMessage json) = U.fromString $ show json


--
-- Html
--

gamePage :: Game -> URL -> Html
gamePage g url =
  thehtml <<
     [ header <<
         [ meta ! [ httpequiv "Content-Type"
                  , content "text/html; charset=UTF-8"
                  ]
         , thetitle << show g
         , jsLib $ pubPath "flaw.js"
         , js $ "configGame(\"" ++ url ++ "\", \"game\");\n"
         ]
     , body ! [ strAttr "onload" "onloadHandler();" ] <<
         [ h1 << show g
         , thediv ! [ identifier "game" ] << "Hello."
         ]
     ]

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]

cssLib path = itag "link" ! [href path, rel "stylesheet", thetype "text/css"]
jsLib path = tag "script" noHtml ! [thetype "text/javascript", src path]
js code = tag "script" (primHtml code) ! [thetype "text/javascript"]

pubPath s = "/pub/" ++ s

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

