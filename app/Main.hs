{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections, Arrows, ViewPatterns, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ((<>))
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad (void, when, forever)
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

import DB

import Debug.Trace

botName :: String
botName = "Yotanido"

declareFields [d|
    data ChatMsg = ChatMsg { chatMsgSender :: Text
                           , chatMsgMessage :: Text
                           } deriving (Show)

    data ChatCommand = ChatCommand { chatCommandSender :: Text
                                   , chatCommandCommand :: Text
                                   , chatCommandArguments :: [Text]
                                   } deriving (Show)
    |]

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
                          liftIO (putStrLn "Keep alive")
                          let response = SBKeepAlivePayload $ keepAlive ^. keepAliveId
                          sendPacket response
                          liftIO (putStrLn "answered")
                      CBPlayerPositionAndLook positionAndLook -> do
                          liftIO $ putStrLn "Position and look"
                          sendPacket $ SBTeleportConfirmPayload $ positionAndLook ^. posLookID
                          sendPacket $ SBPlayerPositionAndLookPayload (positionAndLook^.x) (positionAndLook^.y) (positionAndLook^.z) (positionAndLook^.yaw) (positionAndLook^.pitch) True
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


frpThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> IO ()
frpThread inbound outbound shutdown profile = runSpiderHost $ hostApp app
    where app :: MonadAppHost t m => m ()
          app = do
              (inputEvent, inputFire) <- newExternalEvent
              (tickEvent, tickFire) <- newExternalEvent
              void . liftIO . forkIO . forever $ threadDelay (floor (1/20 * 1000000 :: Double)) >>= tickFire
              void . liftIO . forkIO . forever $ atomically (readTChan inbound) >>= inputFire
              (outEvent, printEvent) <- minecraftBot inputEvent tickEvent
              performEvent_ $ fmap (liftIO . atomically . writeTChan outbound) outEvent
              performEvent_ $ fmap (liftIO . TIO.putStrLn) printEvent
              pure ()


minecraftBot :: (Monad m, Reflex t) => Event t CBPacket -> Event t () -> m (Event t [SBPacket], Event t Text)
minecraftBot inbound tick = pure (never, chatMessageE inbound)

chatMessageE :: Reflex t => Event t CBPacket -> Event t Text
chatMessageE inbound = (flip fmapMaybe) inbound $ \case
    CBChatMessage (CBChatMessagePayload text _) -> Just $ text ^. from network
    _ -> Nothing


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

whileM :: Monad f => f Bool -> f a -> f a
whileM c x = c >>= \case
    False -> x
    True -> x >> whileM c x
