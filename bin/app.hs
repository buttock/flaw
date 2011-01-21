{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad.State
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
import           System.IO.Unsafe (unsafePerformIO)
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
  putMVar theStV $ initSt { stateKey = fromIntegral $ fromEnum $ utctDayTime time }
  Net.withSocketsDo $ SSL.withOpenSSL $ do
    server <- mkHttpServer (cfgPort cfg) Nothing
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
                 , gameLibUrl :: Maybe String
                 }

defaultGame :: Game
defaultGame = Game { gameId = error "uninitialized gameId"
                   , gameStart = error "uninitialized gameStart"
                   , gameEvents = []
                   , gameNEvents = 0
                   , gameEventsReady = error "uninitialized gameEventsReady"
                   , gameLibUrl = Nothing
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
  s <- getSt
  links <- mapM gameLink $ Map.elems (stateGames s)
  ok $ page "Games" $ thediv <<
    [ h2 << "Games"
    , ulist << map (li <<) links
    , thediv << form ! [ action "/games", method "POST" ] <<
        [ submit "start" "Start Game" ]
    ]

startGame :: RouteFn
startGame _ = do
  now <- io getCurrentTime
  ch <- io $ newChan
  game <- modifySt $ \state ->
    let i = stateNextGameId state
        gi = GameId i
        game = defaultGame { gameId = gi, gameStart = now, gameEventsReady = ch }
        games' = Map.insert gi game (stateGames state)
        state' = state { stateNextGameId = succ i
                       , stateGames = games'
                       }
    in return (state', game)
  url <- gameDealerUrl game
  link <- gameDealerLink game
  seeOther url $ page "Game created" $ toHtml link 

getGame :: String -> Bool -> ReqI (Maybe Game)
getGame ident isDealer = do
  s <- getSt
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
  ready <- io $ dupChan (gameEventsReady g)
  _ <- io $ readChan ready
  s <- getSt
  case Map.lookup (gameId g) (stateGames s) of
    Nothing -> badRequest "Game no longer available"
    Just g' -> getEventsFrom g' n

postEvents :: String -> Bool -> RouteFn
postEvents ident isDealer _ = do
  gM <- getGame ident isDealer
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
  return $ hotlink url $ toHtml $ show g

gameDealerLink :: Game -> ReqI HotLink
gameDealerLink g = do
  url <- gameDealerUrl g
  return $ hotlink url $ toHtml $ show g

gameUrl :: Game -> ReqI URL
gameUrl g = do
  s <- getSt
  let (GameId i) = gameId g
      i' :: Integer = fromIntegral $ AES.encrypt (stateKey s) $ fromIntegral i
  return $ "/games/" ++ Base32.encode i'

gameDealerUrl :: Game -> ReqI URL
gameDealerUrl g = do
  s <- getSt
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
         , jsLib $ gamePath "Flaw.js"
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

