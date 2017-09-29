{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections, Arrows #-}
module Main where

import           Control.Applicative (empty)
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens ((^.))
import           Control.Monad (void, forever, when)
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Data.IORef
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import           System.Exit (exitSuccess)
import           FRP.Yampa

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

import Debug.Trace

minecraftThread :: TChan CBPacket -> TChan [SBPacket] -> IO ()
minecraftThread inbound outbound = do
    let uuid = "Your UUID"
        token = "Your auth token"

    putStrLn "Connecting"
    void $ connect "158.69.23.101" Nothing $ do
        liftIO $ putStrLn "Sending handshake"
        handshake
        liftIO $ putStrLn "Handshake sent"

        loginSucc <- login "Yotanido" uuid token

        case loginSucc of
          Left err -> liftIO $ putStrLn err
          Right loginSuccess -> liftIO $ do
              putStrLn "Logged in"

        sendPacket $ SBClientSettingsPayload "en_GB" 1 0 True 0x7F 1
        sendPacket $ SBClientStatusPayload 0
        sendPacket $ SBChatMessagePayload "Beep. Boop. I'm a bot"
        sendPacket $ SBChatMessagePayload "/afk"

        forever $ do
            packetAvailable <- hasPacket
            when packetAvailable $ do
                packet' <- receivePacket

                case packet' of
                  Just packet -> case packet of
                      --PacketUnknown (PacketUnknownPayload bs) -> liftIO . putStrLn . show . BS.unpack $ bs
                      CBKeepAlive keepAlive -> do
                          liftIO (putStrLn "Keep alive")
                          let response = SBKeepAlivePayload $ keepAlive ^. keepAliveId
                          sendPacket response
                          liftIO (putStrLn "answered")
                      CBPlayerPositionAndLook positionAndLook -> do
                          liftIO $ putStrLn "Position and look"
                          sendPacket $ SBTeleportConfirmPayload $ positionAndLook ^. posLookID
                          sendPacket $ SBPlayerPositionAndLookPayload (positionAndLook^.x) (positionAndLook^.y) (positionAndLook^.z) (positionAndLook^.yaw) (positionAndLook^.pitch) True
                      CBChatMessage cmsg -> do
                          liftIO $ putStrLn $ "Chat message: " ++ T.unpack (unNetworkText (cmsg ^. chatMessage))
                      CBDisconnectPlay dc -> do
                          liftIO $ putStrLn $ "Disconnected: " ++ T.unpack (unNetworkText (dc ^. reason))
                      _ -> pure ()
                      -- _ -> liftIO . atomically $ writeTChan inbound packet
                      -- _ -> liftIO $ putMVar inbound packet
                  Nothing -> liftIO $ putStrLn "Connection closed" >> exitSuccess
            --dataToSend <- liftIO . atomically $ tryReadTChan outbound
            --case dataToSend of
            --  Just dataToSend -> sequence_ $ sendPacket <$> dataToSend
            --  Nothing -> pure ()


yampaThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> IO ()
yampaThread inbound outbound shutdown = do
    lastTime <- getCurrentTime >>= newIORef
    reactimate initialize (senseInput lastTime) actuate testSF
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
            (quit ||) <$> readIORef shutdown

testSF :: SF (Event CBPacket) (Bool, Event [SBPacket])
testSF = proc inbound -> do
    msg <- now (SBChatMessage $ SBChatMessagePayload "Test") -< undefined
    quit <- quitMessage -< inbound
    returnA -< (quit, catEvents [msg])

quitMessage :: SF (Event CBPacket) Bool
quitMessage = proc inp -> do
    message <- getChatMessage -< inp
    returnA -< if isEvent message
                  then traceShow message False
                  else False

getChatMessage :: SF (Event CBPacket) (Event Text)
getChatMessage = proc inp -> do
    returnA -< do
        packet <- inp
        case packet of
          CBChatMessage (CBChatMessagePayload t _) -> pure . traceShowId $ unNetworkText t
          _ -> empty

main :: IO ()
main = do
    inbound <- atomically $ newTChan
    outbound <- atomically $ newTChan
    shutdown <- newIORef False
    concurrently_ (minecraftThread inbound outbound) (yampaThread inbound outbound shutdown)
