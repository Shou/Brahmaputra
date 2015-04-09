
-- Copyright 2015 Benedict Aas
-- Licensed under MIT

{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, LiberalTypeSynonyms, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- {{{ Imports

import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Error (readMay, headMay, hush)
import           Control.Exception (try)
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad.State

import           Data.Aeson
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isNothing, fromJust)
import           Data.Monoid ((<>), mempty)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import           Debug.Trace (trace)

import qualified Network.Wreq as Wq
import           Network.WebSockets

import System.Exit (exitSuccess)

-- }}}


-- FIXME relay shouldn't relay to closed websockets.
--      - Hacky solution to this currently enacted, fix.


-- {{{ Constants

debug = True

moodsTable = concat [ "CREATE TABLE IF NOT EXISTS moods "
                    , "( msg TEXT NOT NULL UNIQUE"
                    , ", mood TEXT NOT NULL"
                    , ", rate FLOAT NOT NULL"
                    , ", date DATETIME DEFAULT CURRENT_TIMESTAMP"
                    , ")"
                    ]
moodInsert = "INSERT INTO moods (msg, mood, rate) VALUES (?, ?, ?)"
moodSelect = "SELECT * FROM moods WHERE msg=?"

-- }}}

-- {{{ Data

-- | `$` at the type level.
type f $ a = f a
infixr 2 $

-- | Parentheses wrapping closest pair.
type (~>) = (->)
infixl 5 ~>

-- | Left-associative function type.
type (&>) = (->)
infixl 1 &>


data Game = Game { players :: Map Text Player
                 , chat :: Chat
                 , database :: DB.Connection
                 }
    deriving (Show)

data Player = Player { coords :: Coords Int Int
                     , wsockets :: Map Text (Connection, ThreadId)
                     , moodvar :: MVar Text
                     }
    deriving (Show)

data Coords a b where
    Coords :: (Num a ~ Num b) => !a -> !b -> Coords a b

instance (Show a, Show b) => Show (Coords a b) where
    show (Coords x y) = show x ++ " " ++ show y

instance (Num a ~ Num a') => Field1 (Coords a b) (Coords a' b) a a' where
    _1 k (Coords x y) = (\x' -> Coords x' y) <$> k x

instance (Num b ~ Num b') => Field2 (Coords a b) (Coords a b') b b' where
    _2 k (Coords x y) = (\y' -> Coords x y') <$> k y

data Chat = Chat { history :: [Message] }

-- | Message composed of username, timestamp, and contents.
type Message = (Text, Int, Text)

data Classify =
    Classify { version :: String
             , success :: Bool
             , statusCode :: Int
             , errorMessage :: String
             , mood :: Map String Double
             }

instance FromJSON Classify where
    parseJSON (Object v) = Classify <$> v .: "version"
                                    <*> v .: "success"
                                    <*> v .: "statusCode"
                                    <*> v .: "errorMessage"
                                    <*> v .: "cls1"
    parseJSON _ = mzero

instance Show Connection where
    show _ = "Websocket _"

instance Show DB.Connection where
    show _ = "Database _"

instance Show (MVar a) where
    show _ = "MVar _"

-- }}}

-- {{{ Data utils

defPlayer :: MVar Text -> Player
defPlayer = Player (Coords 0 0) mempty

modGamePlayer :: Player ~> Player -> Text -> Game -> Game
modGamePlayer f user g =
    g { players = M.adjust f user (players g) }

insGamePlayer :: Text -> Player -> Game -> Game
insGamePlayer k p g = g { players = M.insert k p (players g) }

getGamePlayer :: Text -> Game -> Maybe Player
getGamePlayer k g = M.lookup k $ players g

modGame :: TMVar Game -> Game ~> Game -> IO ()
modGame tg f = atomically $ do
    g <- takeTMVar tg
    putTMVar tg $ f g

getGame :: TMVar Game -> IO Game
getGame tg = atomically $ readTMVar tg

coordsTuple :: Coords a b -> (a, b)
coordsTuple (Coords x y) = (x, y)

io :: MonadIO m => IO a -> m a
io = liftIO

-- }}}

-- {{{ Utilities

debugStr :: MonadIO m => String -> m ()
debugStr = liftIO . putStr

debugLine :: MonadIO m => String -> m ()
debugLine = liftIO . putStrLn

traceMod :: Show b => (a -> b) -> a -> a
traceMod f a = trace (show $ f a) a

numNot :: (Eq a, Num a) => a -> a
numNot 0 = 1
numNot _ = 0

numTrunc :: (Eq a, Num a) => a -> a
numTrunc 0 = 0
numTrunc n = 1

-- }}}

-- {{{ API

-- TODO encodeURL
apiURL :: String -> String -> String -> String -> String
apiURL user classifier key text = concat
    [ "http://uclassify.com/browse/", user, "/", classifier, "/ClassifyText?"
    , "readkey=", key, "&output=json&text=", text
    ]

-- TODO remove key
moodAnalyze :: String -> IO $ Map String Double
moodAnalyze text = do
    r <- Wq.get $ apiURL "prfekt" "mood" "loGIvZJZHJfc9aOx6RvLHsHeH0" text

    let body = r ^. Wq.responseBody
        mjson :: Maybe Classify
        mjson = decode body

    flip (maybe $ return mempty) mjson $ \clsfy -> return $ mood clsfy

-- }}}

-- | Game player websockets in two nested `Map's
playerSocks :: (Functor m, MonadIO m) =>
               TMVar Game -> m $ Map Text $ Map Text (Connection, ThreadId)
playerSocks tg = M.map wsockets . players <$> liftIO (atomically $ readTMVar tg)

-- TODO generalize for protocol??
-- | Relay message to other hosts.
relayMessage :: (Functor m, MonadIO m) => TMVar Game -> Text -> Text -> m ()
relayMessage tg p m = M.toList <$> playerSocks tg >>= liftIO . unwrap (getSock send)
  where
    unwrap :: b ~> IO () -> [(a, b)] -> IO ()
    unwrap f m = mapM_ (f . snd) m
    getSock :: a ~> IO () -> Map Text (a, b) -> IO ()
    getSock f = maybe (putStrLn "no") (f . fst) . M.lookup p
    send :: Connection -> IO ()
    send c = handle c $ flip sendTextData m c
    handle :: Connection -> IO () -> IO ()
    handle c io = either (relayPrintError c) return =<< try io
    relayPrintError :: Connection -> ConnectionException -> IO ()
    relayPrintError c _ = putStr "Relay closed: " >> print c

-- | Add player websocket and thread to Game state.
addSocket :: TMVar Game -> Text -> Text -> Connection -> ThreadId -> IO ()
addSocket tg key pcol con tid = do
    io <- atomically $ do
        g <- takeTMVar tg

        let mp = M.lookup key $ players g
            mws = join $ M.lookup pcol . wsockets <$> mp
            -- TODO impure blasphemous exploding code
            p = fromJust mp
            p' = p { wsockets = M.insert pcol (con, tid) $ wsockets p }
            g' = g { players = M.insert key p' $ players g }

        putTMVar tg g'

        return . flip fmap mws $ \(oldcon, oldtid) -> do
            killThread oldtid
            sendClose oldcon ("" :: Text)

    maybe (return ()) id io

-- | Get player's mood MVar
playerMoodvar :: TMVar Game -> Text -> IO $ MVar Text
playerMoodvar t key = do
    g <- atomically $ readTMVar t
    let mp = M.lookup key $ players g
    mv <- maybe newEmptyMVar (return . moodvar) mp
    when (isNothing mp) $ do
        modGame t $ insGamePlayer key $ defPlayer mv
    return mv

-- | Websocket initial thread function
app :: TMVar Game -> PendingConnection -> IO ()
app t p = join . fmap handler . try $ do
    putStr "Incoming request from: "
    print . head . requestHeaders $ pendingRequest p
    c <- acceptRequest p

    (m :: Text) <- receiveData c

    let (pcol : key : _) = map (T.take 10) $ T.words m

    tid <- myThreadId

    -- Initiate mood MVar
    void $ playerMoodvar t key

    putStr "→ " >> print m

    -- TODO use a Map of functions instead. We can addSocket once.
    case pcol of
        "move" -> initMove t c pcol key tid
        "chat" -> initChat t c pcol key tid
        "mood" -> initMood t c pcol key tid
        _ -> sendTextData c ("404 Stream Not Found" :: Text)
  where
    handler :: Either ConnectionException () -> IO ()
    handler (Left e) = print e
    handler _ = return ()

initMove t c pcol key tid = do
    addSocket t key pcol c tid

    cs <- map (over _2 coords) . M.toList . players <$> getGame t
    forM_ cs $ \(ke, co) ->
        sendTextData c . ((ke <> " c ") <>) . T.pack . show $ co

    moveThread c t key

initChat t c pcol key tid = do
    addSocket t key pcol c tid
    chatThread c t key

initMood t c pcol key tid = do
    addSocket t key pcol c tid
    moodThread c t key

-- | Thread and websocket to move the user.
moveThread :: Connection -> TMVar Game -> Text -> IO ()
moveThread c t k = void . flip runStateT 0 . forever $ do
    debugLine "Awaiting coordinates"
    m <- io $ receiveData c

    nt <- io $ floor . (* 1000) <$> getPOSIXTime
    ot <- get
    put nt

    -- XXX
    -- Theoretically time can be equal to nt because of the initial state,
    -- however this is implausible becase the user has to send a stop vector
    -- before a start vector, i.e. (0, 0) before (n, m) where n or m > 0.
    let time = nt - ot
        mt = over both (readMay . T.unpack) $ T.drop 1 <$> T.break (== ' ') m
        (x, y) = over both (numTrunc . maybe 0 ((time *) . numNot)) mt

    io $ modGame t $ flip modGamePlayer k $ \p ->
        let (Coords x' y') = coords p
        in p { coords = Coords (x + x') (y + y') }

    io $ getGamePlayer k <$> getGame t >>= debugLine . show

    io $ relayMessage t "move" $ k <> " v " <> m

-- | Thread and websocket for the user chat.
chatThread :: Connection -> TMVar Game -> Text -> IO ()
chatThread c t k = playerMoodvar t k >>= \mv -> forever $ do
    debugLine "Awaiting chat message"
    (m :: Text) <- receiveData c

    (Game _ db) <- atomically $ readTMVar t

    ts <- floor <$> liftIO getPOSIXTime

    debugStr "Checking local cache database... "
    dms <- DB.quickQuery' db moodSelect [ DB.toSql m ]

    let snd' (x, !y) = y
        sortMoods ms = reverse . sortBy (comparing snd') $ M.toList ms

    moods <- if null dms then do
            debugLine "not found"
            debugLine "Analyzing message mood"

            moods <- moodAnalyze $ T.unpack m

            let mood = fst . head $ sortMoods moods
                rate = snd . head $ sortMoods moods

            DB.run db moodInsert [ DB.toSql m
                                 , DB.toSql mood
                                 , DB.toSql rate
                                 ]
            DB.commit db

            return moods

        else do
            debugLine "data exists"
            debugLine "Retrieving local cache values"

            let mood = DB.fromSql $ dms !! 0 !! 1
                rate = DB.fromSql $ dms !! 0 !! 2

            return $ M.fromList [(mood, rate)]

    let !sortedMoods = sortMoods moods
        fullMsg = (ts, sortedMoods, m)

    putStrLn "Putting mv"
    putMVar mv . T.pack . fst $ head sortedMoods
    print fullMsg

    debugLine $ "The message is: " <> fst (head sortedMoods)

    debugLine $ "Relaying message"
    relayMessage t "chat" $ k <> " " <> T.pack (show ts) <> " " <> m

-- | Thread and websocket for the user mood metadata.
moodThread :: Connection -> TMVar Game -> Text -> IO ()
moodThread c t k = playerMoodvar t k >>= \mv -> forever $ do
    debugLine "Meta thread awaiting mood"
    mood <- takeMVar mv

    relayMessage t "mood" $ k <> " " <> mood

    putStr "→ " >> print mood


main = do
    debugLine "Brahma server v0.1"
    debugLine "Connecting to SQLite3 database `cache'"

    db <- DB.connectSqlite3 "cache"

    debugLine "Initiating table `moods'"

    DB.run db moodsTable []

    gameTM <- newTMVarIO $ Game mempty db

    debugLine "Initiating websocket server"

    forkIO $ runServer "0.0.0.0" 8080 $ app gameTM

    keyboardInput gameTM

  where
    keyboardInput tg = forever $ do
        l <- getLine

        g <- atomically $ readTMVar tg

        case l of
            "game" -> print g
            "players" -> print $ players g
            ('p':'l':'a':'y':'e':'r':' ':key) ->
                print $ getGamePlayer (T.pack key) g

            "socks" -> M.mapKeys (T.take 10) <$> playerSocks tg >>= print
            ":q" -> exitSuccess
            "quit" -> exitSuccess
            _ -> putStrLn "Ask your 'self': what are you trying to achieve?"

