{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import           Control.Monad (void, replicateM_)
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import           Data.Monoid
import qualified Data.Text as Text

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

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
          Right _ -> liftIO $ putStrLn "Logged in"

        replicateM_ 2 $ do
            packet <- receivePacket

            case packet of
              PacketLoginSuccess PacketLoginSuccessPayload{..} -> liftIO . putStrLn . Text.unpack $ "Login success! " <> unNetworkText successUsername <> " " <> unNetworkText uuid
              PacketUnknown (PacketUnknownPayload bs) -> liftIO . putStrLn . show . BS.unpack $ bs
              ConnectionClosed -> liftIO $ putStrLn "Connection closed"
              _ -> pure ()
    pure ()
