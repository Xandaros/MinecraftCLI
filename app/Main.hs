{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections, Arrows, ViewPatterns, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid ((<>))
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad (void, when, forever)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Safe
import           System.Exit (exitSuccess)
import           Reflex hiding (performEvent_)
import           Reflex.Host.App

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

import Commands
import DB

import Debug.Trace

botName :: String
botName = "Yotanido"

minecraftThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> IO ()
minecraftThread inbound outbound shutdown profile = do
    putStrLn "Connecting"

    void $ connect "158.69.23.101" Nothing $ do
        liftIO $ putStrLn "Sending handshake"
        handshake
        liftIO $ putStrLn "Handshake sent"

        loginSucc <- login (profile ^. profileUsername) (profile ^. profileUuid) (profile ^. profileToken)

        case loginSucc of
          Left err -> liftIO $ putStrLn err
          Right _ -> liftIO $ do
              putStrLn "Logged in"

        sendPacket $ SBClientSettingsPayload "en_GB" 1 0 True 0x7F 1
        sendPacket $ SBClientStatusPayload 0
        sendPacket $ SBChatMessage $ SBChatMessagePayload "Beep. Boop. I'm a bot"
        -- sendPacket $ SBChatMessagePayload "/afk"

        whileM (liftIO $ not <$> readIORef shutdown) $ do
            packetAvailable <- hasPacket
            when packetAvailable $ do
                packet' <- receivePacket

                case packet' of
                  Just packet -> case packet of
                      CBKeepAlive keepAlive -> do
                          let response = SBKeepAlivePayload $ keepAlive ^. keepAliveId
                          sendPacket response
                      CBPlayerPositionAndLook positionAndLook -> do
                          liftIO $ putStrLn "Position and look"
                          sendPacket $ SBTeleportConfirmPayload $ positionAndLook ^. posLookID
                          sendPacket $ SBPlayerPositionAndLookPayload (positionAndLook^.x) (positionAndLook^.y) (positionAndLook^.z) (positionAndLook^.yaw) (positionAndLook^.pitch) True
                          liftIO . atomically $ writeTChan inbound packet
                      CBDisconnectPlay dc -> do
                          liftIO $ putStrLn $ "Disconnected: " ++ T.unpack (dc ^. reason . from network)
                      _ -> liftIO . atomically $ writeTChan inbound packet
                  Nothing -> liftIO $ do
                      writeIORef shutdown True
                      putStrLn "Connection closed" >> exitSuccess
            dataToSend <- liftIO . atomically $ tryReadTChan outbound
            case dataToSend of
              Just dataToSend -> sequence_ $ sendPacket <$> dataToSend
              Nothing -> pure ()
            liftIO $ when (not packetAvailable) $ do
                threadDelay (floor $ ((1/21) :: Double) * 1000000)


frpThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> IO ()
frpThread inbound outbound shutdown _profile = runSpiderHost $ hostApp app
    where app :: MonadAppHost t m => m ()
          app = do
              (inputEvent, inputFire) <- newExternalEvent
              (tickEvent, tickFire) <- newExternalEvent
              void . liftIO . forkIO . forever $ threadDelay (floor (1/20 * 1000000 :: Double)) >>= tickFire
              void . liftIO . forkIO . forever $ atomically (readTChan inbound) >>= inputFire
              (outEvent, printEvent, shutdownEvent) <- minecraftBot inputEvent tickEvent
              performEvent_ $ fmap (liftIO . atomically . writeTChan outbound . NE.toList) outEvent
              performEvent_ $ fmap (sequence_ . fmap liftIO . fmap TIO.putStrLn . NE.toList) printEvent
              performEvent_ $ fmap (const . liftIO $ threadDelay 5000 >> writeIORef shutdown True >> exitSuccess) shutdownEvent
              pure ()


minecraftBot :: (MonadHold t m, MonadFix m, Reflex t) => Event t CBPacket -> Event t () -> m (Event t (NonEmpty SBPacket), Event t (NonEmpty Text), Event t ())
minecraftBot inbound tick = do
    let chatMessages = chatMessageE inbound
        commands = commandE chatMessages
    -- ticks <- zipListWithEvent (\a _ -> a) [1..] tick
    -- seconds <- zipListWithEvent (\a _ -> a) ([1..] :: [Integer]) $ ffilter ((==0) . (`mod` (20 :: Integer))) ticks
    playerPos <- playerPositionD inbound (tpCommandE commands)
    pure ( foldl1 (<>) [ (fmap (SBChatMessage . SBChatMessagePayload . view network) <$> commandMessagesE commands)
                       , (mergeList [ whereCommandE commands playerPos
                                    , mkPPAL <$> tag (current playerPos) tick
                                   ])
                       ]
         , mergeList [ T.pack . show <$> chatMessages
                     , T.pack . show <$> commands
                     ]
         , void $ quitCommandE commands
         )

playerPositionD :: (MonadHold t m, Reflex t)
                => Event t CBPacket
                -> Event t (Double, Double, Double)
                -> m (Dynamic t (Double, Double, Double))
playerPositionD inbound tp = do
    let inboundUpdate = fforMaybe inbound $ \case
            CBPlayerPositionAndLook (CBPlayerPositionAndLookPayload x y z _yaw _pitch _flags _) ->
                Just (x ^. from network, y ^. from network, z ^. from network)
            _ -> Nothing
    holdDyn (0, 0, 0) $ leftmost [inboundUpdate, tp]

chatMessageE :: Reflex t => Event t CBPacket -> Event t ChatMsg
chatMessageE inbound = fforMaybe inbound $ \case
    CBChatMessage (CBChatMessagePayload text _) -> fmap decodeChat . chatToString $ text ^. from network
    _ -> Nothing

commandE :: Reflex t => Event t ChatMsg -> Event t ChatCommand
commandE = fmapMaybe (chatToCommand (T.pack botName))

chatToString :: Text -> Maybe String
chatToString c = T.unpack . chatToText . canonicalizeChatComponent <$> decode (BSL.fromStrict $ TE.encodeUtf8 c)

chatToCommand :: Text -> ChatMsg -> Maybe ChatCommand
chatToCommand botName msg = do
    traceM ("Msg: " ++ show msg)
    stripped <- T.stripPrefix (botName <> ": ") (msg ^. message)
    let split = T.words stripped
    cmd <- headMay split
    let args = tailSafe split
    pure . traceShowId $ ChatCommand (msg ^. sender) cmd args

decodeChat :: String -> ChatMsg
decodeChat (stripParagraphs -> s) =
    let sender = if "<" `isPrefixOf` s
                    then takeWhile (/='>') . tail $ s
                    else ""
        message = if "<" `isPrefixOf` s
                     then drop 2 . dropWhile (/='>') $ s
                     else s
    in  ChatMsg (T.pack sender) (T.pack message)

stripParagraphs :: String -> String
stripParagraphs [] = []
stripParagraphs ('§':_:xs) = stripParagraphs xs
stripParagraphs (x:xs) = x:stripParagraphs xs

main :: IO ()
main = do
    profiles <- getActiveProfiles
    when (null profiles) $ do
        putStrLn "No profiles found"
        exitSuccess
    inbound <- atomically $ newTChan
    outbound <- atomically $ newTChan
    shutdown <- newIORef False
    concurrently_ (minecraftThread inbound outbound shutdown (head profiles)) (frpThread inbound outbound shutdown (head profiles))

mkPPAL :: (Double, Double, Double) -> SBPacket
mkPPAL (x, y, z) = SBPlayerPositionAndLook $ SBPlayerPositionAndLookPayload (x ^. network) (y ^. network) (z ^. network) 0 0 True

whileM :: Monad f => f Bool -> f () -> f ()
whileM c x = c >>= \case
    False -> pure ()
    True -> x >> whileM c x

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
