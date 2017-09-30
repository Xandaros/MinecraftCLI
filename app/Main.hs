{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections, Arrows, ViewPatterns, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative (empty)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad (void, when)
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Data.IORef
import           Data.List (isPrefixOf, stripPrefix)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import           Safe
import           System.Exit (exitSuccess)
import           Text.JSON.JPath
import           FRP.Yampa
import           FRP.Yampa.Event

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

import DB

import Debug.Trace

botName :: String
botName = "Yotanido"

declareFields [d|
    data ChatMsg = ChatMsg { chatMsgSender :: String
                           , chatMsgMessage :: String
                           } deriving (Show)

    data ChatCommand = ChatCommand { chatCommandSender :: String
                                   , chatCommandCommand :: String
                                   , chatCommandArguments :: [String]
                                   } deriving (Show)
    |]

minecraftThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> IO ()
minecraftThread inbound outbound shutdown profile = do
    putStrLn "Connecting"
    void $ connect "minerva2gb.fluctis.com" Nothing $ do
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


yampaThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> IO ()
yampaThread inbound outbound shutdown profile = do
    lastTime <- getCurrentTime >>= newIORef
    reactimate initialize (senseInput lastTime) actuate (mainSF profile)
    where
        initialize :: IO (Event CBPacket)
        initialize = pure NoEvent
        senseInput :: IORef UTCTime -> Bool -> IO (DTime, Maybe (Event CBPacket))
        senseInput lastTime _ = do
            curTime <- getCurrentTime
            diffTime <- realToFrac . diffUTCTime curTime <$> readIORef lastTime
            writeIORef lastTime curTime
            inPacket <- atomically $ tryReadTChan inbound
            case inPacket of
              Nothing -> pure (diffTime, Just NoEvent)
              Just packet -> pure (diffTime, Just (Event packet))
        actuate :: Bool -> (Bool, Event [SBPacket]) -> IO Bool
        actuate _ (quit, output) = do
            case output of
              NoEvent -> pure ()
              Event outp -> atomically $ writeTChan outbound outp
            when quit $ void (writeIORef shutdown True)
            readIORef shutdown

mainSF :: Profile -> SF (Event CBPacket) (Bool, Event [SBPacket])
mainSF profile = proc inbound -> do
    chat <- getChatMessage -< inbound
    cmds <- commands (profile ^. profileUsername) -< chat
    quit <- quitMessage -< cmds
    ping <- pingMessage -< cmds
    returnA -< (quit, catEvents [ping])

quitMessage :: SF (Event ChatCommand) Bool
quitMessage = arr $ isEvent . filterE ((=="quit") . view command)

pingMessage :: SF (Event ChatCommand) (Event SBPacket)
pingMessage = arr $ \msg -> do
    ping <- filterE ((=="ping") . view command) msg
    let response = SBChatMessage (SBChatMessagePayload $ (ping ^. sender ++ ": pong") ^. to T.pack . network)
    pure response

commands :: Text -> SF (Event ChatMsg) (Event ChatCommand)
commands botName = arr $ mapFilterE (chatToCommand botName)

getChatMessage :: SF (Event CBPacket) (Event ChatMsg)
getChatMessage = arr $ \inp -> do
    packet <- inp
    case packet of
      CBChatMessage msg -> maybeToEvent . fmap decodeChat . chatToString $ msg ^. chatMessage . from network
      _ -> empty

chatToString :: Text -> Maybe String
chatToString = fmap read . headMay . jPath ("/extra[0]/text" :: String) . T.unpack

chatToCommand :: Text -> ChatMsg -> Maybe ChatCommand
chatToCommand botName msg = do
    traceM ("Msg: " ++ show msg)
    stripped <- stripPrefix (T.unpack botName ++ ": ") (msg ^. message)
    traceM stripped
    let split = words stripped
    cmd <- headMay split
    traceM cmd
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
    in ChatMsg sender message

stripParagraphs :: String -> String
stripParagraphs [] = []
stripParagraphs ('§':_:xs) = xs
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
    concurrently_ (minecraftThread inbound outbound shutdown (head profiles)) (yampaThread inbound outbound shutdown (head profiles))

whileM :: Monad f => f Bool -> f a -> f a
whileM c x = c >>= \case
    False -> x
    True -> x >> whileM c x
