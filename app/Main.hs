{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import           Control.Monad (void, forever)
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
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
          Right PacketLoginSuccessPayload{..} -> liftIO $ do
              putStrLn "Logged in"
              print uuid
              print successUsername

        forever $ do
            packet <- receivePacket

            case packet of
              --PacketUnknown (PacketUnknownPayload bs) -> liftIO . putStrLn . show . BS.unpack $ bs
              PacketKeepAlive ka -> liftIO (putStrLn "Keep alive") >> sendPacket ka
              PacketCBPlayerPositionAndLook PacketCBPlayerPositionAndLookPayload{..} ->
                  sendPacket $ PacketTeleportConfirmPayload posLookCBID
              ConnectionClosed -> liftIO $ putStrLn "Connection closed" >> exitSuccess
              _ -> pure ()
