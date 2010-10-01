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
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import Server
import Message

type L = L.ByteString
type S = S.ByteString


main :: IO ()
main = do
  stateM <- newMVar initState
  server 8000 (handleRequest stateM)

--
-- Game state
--

newtype GameId = GameId S
  deriving (Eq, Ord)

instance Show GameId where
  show (GameId s) = S.unpack s

data Game = Game { gameId :: GameId }

instance Show Game where
  show g = "Game " ++ (show $ gameId g)

data State = State { stateGames :: Map GameId Game
                   , stateNextId :: Integer
                   }

initState :: State
initState = State { stateGames = Map.empty
                  , stateNextId = 0
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
          _ -> notFound'
      _ -> notFound'

  gamesRequest path =
    case path of
      [] -> case S.unpack $ reqMethod req of 
              "GET" -> showGames
              "POST" -> startGame
              _ -> badRequest "Method not allowed."
      (i:restPath) -> do
        state <- getState
        let gi = GameId i
        case Map.lookup gi (stateGames state) of
          Nothing -> notFound $ "no such game: " ++ show gi
          Just game -> gameRequest game restPath

  startGame = do
    game <- io $ modifyMVar stateM $ \state ->
      let i = stateNextId state
          gi = GameId $ S.pack $ show i
          game = Game gi
          games' = Map.insert gi game (stateGames state)
          state' = state { stateNextId = i + 1
                         , stateGames = games'
                         }
      in return (state', game)
    seeOther (gameUrl game) $ page "Game created" $ toHtml $ gameLink game

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
    state <- getState
    ok $ page "Games" $ thediv <<
      [ h2 << "Games"
      , ulist << map ((li <<) . gameLink) (Map.elems $ stateGames state)
      , thediv << form ! [ action "/games", method "POST" ] <<
          [ submit "start" "Start Game" ]
      ]

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
-- Links
--

gameUrl :: Game -> URL
gameUrl g = "/games/" ++ show (gameId g)

gameLink :: Game -> HotLink
gameLink g = hotlink (gameUrl g) $ toHtml $ show (gameId g)

--
-- Html
--

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]
