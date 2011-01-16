{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad (when)
import           Control.Monad.State
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
import           Data.Monoid
import           Data.Time.Clock
import           Data.Maybe (isJust, fromMaybe)
import           System.FilePath.Posix (joinPath, combine, normalise, isRelative)
import qualified System.IO as IO
import           System.IO.Unsafe (unsafePerformIO)
import           Text.JSON
import           Text.XHtml.Strict hiding (p)

import qualified OpenSSL as SSL
import qualified Network.Socket as Net
import HttpServer

type L = L.ByteString
type S = S.ByteString

encodeJSValue :: JSValue -> String
encodeJSValue = encode

main :: IO ()
main = do
  time <- getCurrentTime
  putMVar theStV $ initSt { stateKey = fromIntegral $ fromEnum $ utctDayTime time }
  Net.withSocketsDo $ SSL.withOpenSSL $ do
    server <- mkHttpServer 8000 Nothing
    inOtherThread $ runHttpServer server systemRoute

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

data St = St { stateGames :: Map GameId Game
             , stateNextGameId :: Integer
             , stateKey :: Word128
             }

initSt :: St
initSt = St { stateGames = Map.empty
            , stateNextGameId = 0
            , stateKey = 42
            }

--
-- Request-processing environment
--

theStV :: MVar St
theStV = unsafePerformIO $ newEmptyMVar

getTheSt :: IO St
getTheSt = readMVar theStV

type ReqM = IO

type ReqI a = Iter L ReqM a
type ReqIResp = ReqI (HttpResp ReqM)

type RouteFn = HttpReq -> ReqIResp

getSt :: ReqI St
getSt = lift $ getTheSt

modifySt :: (St -> IO (St, a)) -> ReqI a
modifySt f = io $ modifyMVar theStV f

io :: IO a -> ReqI a
io = liftIO

--
-- Request handler
--

systemRoute :: HttpRoute ReqM
systemRoute = 
  routeMap [("games", gamesRoute)
           ,("dealer", routeVar dealerRoute)
           ,("pub", pubRoute)
           ]

pubRoute = routeMethod "GET" $ routeFn $ \req ->
      case safeFilePath ("pub" : map S.unpack (reqPathLst req)) of
        Nothing -> badRequest "Bad file path"
        Just filePath -> okJSFile filePath

okJSFile path = ok path

okJSON json = ok $ U.fromString $ encodeJSValue json

gamesRoute :: HttpRoute ReqM
gamesRoute =
  mconcat [ routeTop $ mconcat [ routeMethod "GET" $ routeFn showGames
                               , routeMethod "POST" $ routeFn startGame
                               ]
          -- , routeVar $ gameRoute
          ]

showGames :: RouteFn
showGames req = do
  s <- getSt
  links <- mapM gameLink $ Map.elems (stateGames s)
  ok $ page "Games" $ thediv <<
    [ h2 << "Games"
    , ulist << map (li <<) links
    , thediv << form ! [ action "/games", method "POST" ] <<
        [ submit "start" "Start Game" ]
    ]

startGame :: RouteFn
startGame req = do
  now <- io getCurrentTime
  ch <- io $ newChan
  (s, game) <- modifySt $ \state ->
    let i = stateNextGameId state
        gi = GameId i
        game = defaultGame { gameId = gi, gameStart = now, gameEventsReady = ch }
        games' = Map.insert gi game (stateGames state)
        state' = state { stateNextGameId = succ i
                       , stateGames = games'
                       }
    in return (state', (state', game))
  seeOther (gameDealerUrl s game) $ page "Game created" $ toHtml $ gameDealerLink s game

dealerRoute = routeReq $ \req ->
  let ident = S.unpack $ head $ reqPathParams req
  in gameRoute ident True

getGame :: String -> Maybe Game
getGame ident = do
  s <- getSt
  let ii' :: Integer = fromMaybe 0 $ Base32.decode $ ident
      i' :: Word128 = fromIntegral ii'
      i :: Integer = fromIntegral $ AES.decrypt (stateKey s + 1) i'
  let gi = GameId i
  return $ Map.lookup gi (stateGames s)

gameRoute ident isDealer =
  mconcat [ routeTop $ routeFn $ showGame ident isDealer
          , routeMap [("events", mconcat [ routeMethod "GET" $ getEvents ident
                                         , routeMethod "POST" $ routeTop $ routeFn $ postEvents ident
                                         ])]
          ]

showGame :: String -> Bool -> RouteFn
showGame ident isDealer req = do
  gM <- getGame ident
  case gM of
    Nothing -> notFound "no such game"
    Just g -> do
      s <- getSt
      let url = (if isDealer then gameDealerUrl else gameUrl) s g
      ok $ gamePage g url isDealer

getEvents :: String -> HttpRoute ReqM
getEvents ident = routeVar $ routeTop $ routeFn $ getEventsFrom ident

getEventsFrom :: String -> RouteFn
getEventsFrom ident req = do
  gM <- getGame ident
  case gM of
    Nothing -> notFound "no such game"
    Just g ->
      let n = S.unpack $ head $ reqPathParams req
          events = gameEventsFrom n g
      in if null events
          then do
            ready <- io $ dupChan (gameEventsReady g)
            _ <- io $ readChan ready
            getEventsFrom' g n  
          else
            okJSON $ showJSON events

getEventsFrom' g n = do
  gM <- getGame $ gameId g
  case gM of
    Nothing -> badRequest "Game no longer available"
    Just g' -> getEventsFrom g' n

postEvents :: String -> RouteFn
postEvents ident req = do
  gM <- getGame ident
  case gM of
    Nothing -> notFound "no such game"
    Just g -> do
      -- io $ threadDelay $ 2 * 1000 * 1000
      eventsS <- netstringI
      let events = either error id $ resultToEither $ decode $
                     U.toString eventsS
      let gi = gameId g
      gameM <- modifySt $ \s ->
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

--
-- Url
--

gameLink :: Game -> ReqI HotLink
gameLink g = do
  url <- gameUrl g
  return $ hotlink url (toHtml $ show g)

gameUrl :: Game -> ReqI URL
gameUrl g = do
  s <- getSt
  let (GameId i) = gameId g
      i' :: Integer = fromIntegral $ AES.encrypt (stateKey s) $ fromIntegral i
  return $ "/games/" ++ Base32.encode i'

gameDealerLink :: St -> Game -> HotLink
gameDealerLink s g = hotlink (gameDealerUrl s g) $ toHtml $ show g

gameDealerUrl :: St -> Game -> URL
gameDealerUrl s g = "/dealer/" ++ Base32.encode i'
 where
  (GameId i) = gameId g
  i' :: Integer = fromIntegral $ AES.encrypt (stateKey s + 1) $ fromIntegral i

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
 
ok :: (HTML h) => h -> ReqI (HttpResp ReqM)
ok h = return $ mkHtmlResp stat200 (U.fromString $ renderHtml $ toHtml h)

seeOther :: URL -> msg -> ReqI (HttpResp ReqM)
seeOther url _ = return $ resp303 url

notFound msg = ok $ toHtml msg

badRequest msg = ok $ toHtml msg

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

inOtherThread :: IO () -> IO ()
inOtherThread io = do
  sem <- newQSem 0
  _ <- forkIO $ io `finally` signalQSem sem
  waitQSem sem

