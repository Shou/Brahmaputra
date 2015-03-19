
-- Author: Benedict Aas

{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- {{{ Imports

import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Error (readMay, headMay)
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad.State

import           Data.Aeson
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>), mempty)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB

import qualified Network.Wreq as Wq
import           Network.WebSockets

-- }}}

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

type f $ a = f a
infixr 0 $

data Game = Game { players :: Map Text Player
                 , database :: DB.Connection
                 }
    deriving (Show)

data Player = Player { key :: Text
                     , coords :: !Coords
                     , wsockets :: Map Text (Connection, ThreadId)
                     , moodvar :: MVar Text
                     }
    deriving (Show)

data Coords = Coords { x :: !Int, y :: !Int }

instance Show Coords where
    show (Coords x y) = show x ++ "," ++ show y

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
defPlayer = Player mempty (Coords 0 0) mempty

modGamePlayer :: (Player -> Player) -> Text -> Game -> Game
modGamePlayer f user g =
    g { players = M.adjust f user (players g) }

insGamePlayer :: Text -> Player -> Game -> Game
insGamePlayer k p g = g { players = M.insert k p (players g) }

stateGame :: TMVar Game -> (Game -> Game) -> IO ()
stateGame tg f = atomically $ do
    g <- takeTMVar tg
    putTMVar tg $ f g

-- }}}

-- {{{ Utilities

debugStr :: MonadIO m => String -> m ()
debugStr = liftIO . putStr

debugLine :: MonadIO m => String -> m ()
debugLine = liftIO . putStrLn

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
playerChatSocks :: TMVar Game -> IO $ Map Text $ Map Text (Connection, ThreadId)
playerChatSocks tg = M.fromList
                   . map (over _2 wsockets)
                   . M.toList
                   . players <$> atomically (readTMVar tg)

addSocket :: TMVar Game -> Text -> Text -> Connection -> ThreadId -> IO ()
addSocket tg key pcol con tid = do
    io <- atomically $ do
        g <- takeTMVar tg
        let mp = M.lookup key $ players g
            mws = join $ M.lookup pcol . wsockets <$> mp
            g' = g
        putTMVar tg g'
        return . flip fmap mws $ \(oldcon, oldtid) -> do
            killThread oldtid
            sendClose oldcon ("" :: Text)

    maybe (return ()) id io

playerMoodvar :: TMVar Game -> Text -> IO $ MVar Text
playerMoodvar t key = do
    g <- atomically $ readTMVar t
    let mp = M.lookup key $ players g
    mv <- maybe newEmptyMVar (return . moodvar) mp
    when (isNothing mp) $ do
        stateGame t $ insGamePlayer key $ defPlayer mv
    return mv

-- TODO get ThreadID so we can KILL old threads
app :: TMVar Game -> PendingConnection -> IO ()
app t p = do
    putStr "Incoming request from: "
    print . head . requestHeaders $ pendingRequest p
    c <- acceptRequest p
    (m :: Text) <- receiveData c

    let (protocol : key : _) = T.words m

    tid <- myThreadId

    -- Initiate mood MVar
    void $ playerMoodvar t key

    putStr "→ " >> print m

    -- TODO use a Map of functions instead. We can addSocket once.
    case protocol of
        "move" -> addSocket t key protocol c tid >> moveThread c t key
        "chat" -> addSocket t key protocol c tid >> chatThread c t key
        "mood" -> addSocket t key protocol c tid >> moodThread c t key
        _ -> sendTextData c ("404 Stream Not Found" :: Text)

-- FIXME undefined used
moveThread :: Connection -> TMVar Game -> Text -> IO ()
moveThread c t k = void . flip runStateT (defPlayer undefined) $ forever $ do
    debugLine "Awaiting coordinates"
    m <- liftIO $ receiveData c

    (Coords ox oy) <- coords <$> get

    let (tx, ty) = T.drop 1 <$> T.break (== ',') m
        (mx, my) = (readMay $ T.unpack tx, readMay $ T.unpack ty)
        (x, y) = (maybe 0 id mx + ox, maybe 0 id my + oy)

    modify $ \s -> s { coords = Coords x y }

    debugLine $ show x <> " x " <> show y

    get >>= liftIO . sendTextData c . T.pack . show . coords

chatThread :: Connection -> TMVar Game -> Text -> IO ()
chatThread c t k = playerMoodvar t k >>= \mv -> forever $ do
    debugLine "Awaiting chat message"
    (m :: Text) <- receiveData c

    (Game _ db) <- atomically $ readTMVar t

    ts <- liftIO getPOSIXTime

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

moodThread :: Connection -> TMVar Game -> Text -> IO ()
moodThread c t k = playerMoodvar t k >>= \mv -> forever $ do
    debugLine "Meta thread awaiting mood"
    mood <- takeMVar mv

    sendTextData c mood

    putStr "→ " >> print mood


main = do
    debugLine "Brahma server v0.1"
    debugLine "Connecting to SQLite3 database `cache'"

    db <- DB.connectSqlite3 "cache"

    debugLine "Initiating table `moods'"

    DB.run db moodsTable []

    gameTM <- newTMVarIO $ Game mempty db

    debugLine "Initiating websocket server"

    runServer "0.0.0.0" 8080 $ app gameTM

