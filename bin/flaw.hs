{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32
import           Control.Concurrent
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
  stateM <- newMVar $ initSt { stateKey = fromIntegral $ fromEnum $ utctDayTime time }
  server 8000 systemRoute (runReqM stateM)

runReqM :: MVar St -> ReqM a -> IO a
runReqM stateM m = modifyMVar stateM $ \state -> do
    (x, state') <- runStateT m state
    return (state', x)

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

type ReqM = StateT St IO

type ReqI a = Iter L ReqM a
type ReqIResp = ReqI (HttpResp ReqM)

type RouteFn = HttpReq -> ReqIResp

getSt :: ReqI St
getSt = lift $ get

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
 
ok :: Html -> ReqI (HttpResp ReqM)
ok html = return $ mkHtmlResp stat200 (U.fromString $ renderHtml html)

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

