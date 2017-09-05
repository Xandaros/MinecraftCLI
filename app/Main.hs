{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import           Control.Monad (void, forever)
import           Control.Monad.IO.Class
import System.Exit (exitSuccess)

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet

main :: IO ()
main = do
    let uuid = "Your UUID"
        token = "Your auth token"

    putStrLn "Connecting"
    void $ connect "104.219.4.7" Nothing $ do
        liftIO $ putStrLn "Sending handshake"
        handshake
        liftIO $ putStrLn "Handshake sent"

        loginSucc <- login "Yotanido" uuid token

        case loginSucc of
          Left err -> liftIO $ putStrLn err
          Right CBLoginSuccessPayload{..} -> liftIO $ do
              putStrLn "Logged in"
              print uuid
              print successUsername

        sendPacket $ SBClientSettingsPayload "" 2 0 False 0x7F 1
        sendPacket $ SBChatMessagePayload "Beep. Boop. I'm a bot"
        sendPacket $ SBChatMessagePayload "/afk"

        forever $ do
            packet' <- receivePacket

            case packet' of
              Just packet -> case packet of
                  --PacketUnknown (PacketUnknownPayload bs) -> liftIO . putStrLn . show . BS.unpack $ bs
                  CBKeepAlive CBKeepAlivePayload{..} -> do
                      liftIO (putStrLn "Keep alive")
                      let response = SBKeepAlivePayload keepAliveId
                      sendPacket response
                      liftIO $ print keepAliveId
                      liftIO $ print response
                      liftIO (putStrLn "answered")
                  CBPlayerPositionAndLook CBPlayerPositionAndLookPayload{..} -> do
                      liftIO $ putStrLn "Position and look"
                      sendPacket $ SBTeleportConfirmPayload posLookCBID
                  _ -> pure ()
              Nothing -> liftIO $ putStrLn "Connection closed" >> exitSuccess
