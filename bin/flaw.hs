{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IterIO
import           Data.IterIO.Http
-- import           Data.IterIO.Zlib
-- import           Data.List (intersperse)
-- import qualified Data.ListLike as LL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import           Data.LargeWord (Word128)
import           Data.Time.Clock
import qualified Codec.Encryption.AES as AES
import qualified Codec.Crockford as Base32

import Server
import Message

type L = L.ByteString
-- type S = S.ByteString


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

data Game = Game { gameId :: GameId
                 , gameStart :: UTCTime
                 }

instance Show Game where
  show game = show $ gameStart game

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
          _ -> notFound'
      _ -> notFound'

  dealerRequest path =
    case path of
      (is:restPath) -> do
        s <- getState
        let ii' :: Integer = fromMaybe 0 $ Base32.decode $ S.unpack is
            i' :: Word128 = fromIntegral ii'
            i :: Integer = fromIntegral $ AES.decrypt (stateKey s + 1) i'
        let gi = GameId i
        case Map.lookup gi (stateGames s) of
          Nothing -> notFound $ "No such game."
          Just game -> gameRequest game restPath

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
          Nothing -> notFound $ "No such game."
          Just game -> gameRequest game restPath

  startGame = do
    now <- io getCurrentTime
    (s, game) <- io $ modifyMVar stateM $ \state ->
      let i = stateNextGameId state
          gi = GameId i
          game = Game { gameId = gi, gameStart = now }
          games' = Map.insert gi game (stateGames state)
          state' = state { stateNextGameId = succ i
                         , stateGames = games'
                         }
      in return (state', (state', game))
    seeOther (gameDealerUrl s game) $ page "Game created" $ toHtml $ gameDealerLink s game

  gameRequest g path =
    case path of
      [] -> showGame g
      {-
      [x] | x == "events" -> 
        case S.unpack $ reqMethod req of
          "GET" -> getEvents
          "POST" -> postEvents
          _ -> error "Bad request"
      -}
      _ -> notFound'

  showGames = do
    s <- getState
    ok $ page "Games" $ thediv <<
      [ h2 << "Games"
      , ulist << map ((li <<) . gameLink s) (Map.elems $ stateGames s)
      , thediv << form ! [ action "/games", method "POST" ] <<
          [ submit "start" "Start Game" ]
      ]

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

  showGame g = ok $ thediv << [ h2 << show g ]

  respondI response = inumPure response .| handleI h
  
  xhtmlResponseI status headers x = respondI $ xhtmlResponse status headers (toHtml x)

  ok = xhtmlResponseI statusOK []
  seeOther url = xhtmlResponseI statusSeeOther ["Location: " ++ url]
  notFound = xhtmlResponseI statusNotFound []
  notFound' = notFound "Not Found."
  badRequest = xhtmlResponseI statusBadRequest []

  getState = io $ readMVar stateM

io :: (MonadIO m) => IO a -> m a
io x = liftIO x

--
-- Html
--

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]
