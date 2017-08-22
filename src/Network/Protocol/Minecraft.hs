{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Network.Protocol.Minecraft where

import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Data.Text (Text)
import Data.Word
import Control.Monad.State
import GHC.IO.Handle
import qualified Network.Protocol.Minecraft.Encoding as Encoding
import Network.Protocol.Minecraft.Encoding ( EncodedT, runEncodedT, defaultEncodingState, generateSharedKey
                                           , createServerHash, enableEncryption, setCompressionThreshold
                                           )
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types
import qualified Network.Protocol.Minecraft.Yggdrasil as Yggdrasil
import Network.Socket as Socket
import System.IO (IOMode(..))

newtype Minecraft a = Minecraft { unMinecraft :: StateT MinecraftState (EncodedT IO) a
                                } deriving (Functor, Applicative, Monad, MonadIO)

data MinecraftState = MinecraftState { connectionState :: ConnectionState
                                     , dimension :: Dimension
                                     , difficulty :: Word8
                                     , gamemode :: Word8
                                     , mc_server :: HostName
                                     , mc_port :: PortNumber
                                     }

defaultMinecraftState :: MinecraftState
defaultMinecraftState = MinecraftState { connectionState = Handshaking
                                       , dimension = Overworld
                                       , difficulty = 2
                                       , gamemode = 0
                                       , mc_server = ""
                                       , mc_port = 25565
                                       }

runMinecraft :: Handle -> MinecraftState -> Minecraft a -> IO a
runMinecraft handle state = fmap fst . runEncodedT (defaultEncodingState handle) . flip evalStateT state . unMinecraft

getState :: Minecraft MinecraftState
getState = Minecraft get

getStates :: (MinecraftState -> a) -> Minecraft a
getStates = Minecraft . gets

getConnectionState :: Minecraft ConnectionState
getConnectionState = Minecraft $ gets connectionState

receivePacket :: Minecraft CBPacket
receivePacket = do
    connState <- getConnectionState
    packet <- liftMC $ Encoding.readPacket connState
    pure packet

sendPacket :: (HasPacketID a, Binary a) => a -> Minecraft ()
sendPacket p = liftMC $ Encoding.sendPacket p

connect :: HostName -> Maybe PortNumber -> Minecraft a -> IO (Either String a)
connect host port' mc = do
    let port = fromMaybe 25565 port'
    addrs <- getAddrInfo Nothing (Just host) (Just $ show port)
    if null addrs
       then pure . Left $ "Unable to find host " ++ host
       else do
           sock <- socket AF_INET Stream defaultProtocol
           print (addrAddress $ addrs !! 0)
           Socket.connect sock (addrAddress $ addrs !! 0)
           handle <- socketToHandle sock ReadWriteMode
           Right <$> runMinecraft handle (defaultMinecraftState{mc_server = host, mc_port = port}) mc <* hClose handle

handshake :: Minecraft ()
handshake = do
    host <- getStates mc_server
    port <- getStates mc_port
    sendPacket $ PacketHandshakePayload 335 (NetworkText $ Text.pack host) (fromIntegral port) LoggingIn
    setConnectionState LoggingIn

login :: Text -> Text -> Text -> Minecraft (Either String PacketLoginSuccessPayload)
login username uuid token = do
    sendPacket $ PacketLoginStartPayload (NetworkText username)
    PacketEncryptionRequest encRequest <- receivePacket
    sharedSecret <- liftIO $ generateSharedKey
    let serverHash = createServerHash (unNetworkText $ serverID encRequest) sharedSecret (pubKey encRequest)
        joinRequest = Yggdrasil.JoinRequest token uuid (Text.pack serverHash)
    joinSucc <- liftIO $ Yggdrasil.join joinRequest
    if not joinSucc
       then pure . Left $ "Unable to authenticate with mojang servers"
       else do
           Just response <- liftIO $ Encoding.encryptionResponse sharedSecret encRequest
           sendPacket response
           encSucc <- liftMC $ enableEncryption sharedSecret
           if not encSucc
              then pure . Left $ "Unable to enable encryption"
              else do
                  loginSuccPacket <- whileM $ do
                      packet <- receivePacket
                      case packet of
                        PacketSetCompression (PacketSetCompressionPayload thresh) ->
                            liftMC $ setCompressionThreshold (fromIntegral thresh) >> pure Nothing
                        PacketLoginSuccess x -> setConnectionState Playing >> pure (Just (Right x))
                        _ -> pure . Just $ Left "Unexpected packet received during login"
                  PacketJoinGame (PacketJoinGamePayload{..}) <- receivePacket
                  setGamemode joinGamemode
                  setDifficulty joinDifficulty
                  setDimension joinDimension
                  pure loginSuccPacket

setConnectionState :: ConnectionState -> Minecraft ()
setConnectionState c = Minecraft $ modify $ \s -> s{connectionState = c}

setGamemode :: Word8 -> Minecraft ()
setGamemode gm = Minecraft $ modify $ \s -> s{gamemode = gm}

setDifficulty :: Word8 -> Minecraft ()
setDifficulty dif = Minecraft $ modify $ \s -> s{difficulty = dif}

setDimension :: Dimension -> Minecraft ()
setDimension dim = Minecraft $ modify $ \s -> s{dimension = dim}

liftMC :: EncodedT IO a -> Minecraft a
liftMC = Minecraft . lift

whileM :: Monad m => m (Maybe a) -> m a
whileM action = do
    cont <- action
    case cont of
      Nothing -> whileM action
      Just x -> pure x
