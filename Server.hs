
-- Author: Benedict Aas

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


-- {{{ Imports

import           Control.Applicative ((<$>))
import           Control.Concurrent (ThreadId, myThreadId, forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Error (readMay)
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad.State

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.WebSockets

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

-- }}}

defPlayer = Player (Coords 0 0) mempty

-- TODO get ThreadID so we can KILL old threads
app :: TMVar Game -> PendingConnection -> IO ()
app tv p = do
    print $ pendingRequest p
    c <- acceptRequest p
    m <- receiveData c

    print m

    case (m :: Text) of
        "coords" -> coordsThread c
        "chat" -> void $ return c
        _ -> sendTextData c ("404 Stream Not Found" :: Text)

coordsThread :: Connection -> IO ()
coordsThread c = do
    void . flip runStateT defPlayer $ do
        forever $ do
            m <- liftIO $ receiveData c

            (Coords ox oy) <- coords <$> get

            let (tx, ty) = T.drop 1 <$> T.break (== ',') m
                (mx, my) = (readMay $ T.unpack tx, readMay $ T.unpack ty)
                (x, y) = (maybe 0 id mx + ox, maybe 0 id my + oy)

            modify $ \s -> s { coords = Coords x y }

            liftIO . print $ show x <> " x " <> show y

            get >>= liftIO . sendTextData c . T.pack . show . coords


main = do
    threadsVar <- newTMVarIO $ Game mempty
    runServer "0.0.0.0" 8080 $ app threadsVar

