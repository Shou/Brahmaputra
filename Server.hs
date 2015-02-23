
-- Author: Benedict Aas

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- {{{ Imports

import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (ThreadId, myThreadId, forkIO)
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
import           Data.Monoid ((<>), mempty)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Network.Wreq as Wq
import           Network.WebSockets

-- }}}

-- {{{ Constants

debug = True

-- }}}

-- {{{ Data

data Game = Game { players :: Map Text Player }
    deriving (Show)

data Player = Player { coords :: !Coords
                     , threads :: Map Text ThreadId
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

-- }}}

defPlayer = Player (Coords 0 0) mempty

-- {{{ Utilities

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
moodAnalyze :: String -> IO (Map String Double)
moodAnalyze text = do
    r <- Wq.get $ apiURL "prfekt" "mood" "loGIvZJZHJfc9aOx6RvLHsHeH0" text

    let body = r ^. Wq.responseBody
        mjson :: Maybe Classify
        mjson = decode body

    flip (maybe $ return mempty) mjson $ \clsfy -> return $ mood clsfy

-- }}}

-- TODO get ThreadID so we can KILL old threads
app :: TMVar Game -> PendingConnection -> IO ()
app tv p = do
    putStr "Incoming request from: "
    print . headMay . requestHeaders $ pendingRequest p
    c <- acceptRequest p
    m <- receiveData c

    putStr "→ "
    print m

    case (m :: Text) of
        "coords" -> coordsThread c
        "chat" -> chatThread c
        _ -> sendTextData c ("404 Stream Not Found" :: Text)

coordsThread :: Connection -> IO ()
coordsThread c = void . flip runStateT defPlayer $ forever $ do
    debugLine "Awaiting coordinates"
    m <- liftIO $ receiveData c

    (Coords ox oy) <- coords <$> get

    let (tx, ty) = T.drop 1 <$> T.break (== ',') m
        (mx, my) = (readMay $ T.unpack tx, readMay $ T.unpack ty)
        (x, y) = (maybe 0 id mx + ox, maybe 0 id my + oy)

    modify $ \s -> s { coords = Coords x y }

    debugLine $ show x <> " x " <> show y

    get >>= liftIO . sendTextData c . T.pack . show . coords

chatThread :: Connection -> IO ()
chatThread c = void . flip runStateT [] $ forever $ do
    debugLine "Awaiting chat message"
    (m :: Text) <- liftIO $ receiveData c

    ts <- liftIO getPOSIXTime

    debugLine "Analyzing message mood..."
    moods <- liftIO $ moodAnalyze $ T.unpack m

    let sortedMoods = reverse . sortBy (comparing snd) $ M.toList moods
        fullMsg = (ts, sortedMoods, m)

    liftIO $ print fullMsg

    debugLine $ "The message is: " <> fst (head sortedMoods)

    history <- get

    modify (++ [fullMsg])


main = do
    debugLine "Brahma server v0.1"
    threadsVar <- newTMVarIO $ Game mempty
    debugLine "Initiating websocket server"
    runServer "0.0.0.0" 8080 $ app threadsVar

