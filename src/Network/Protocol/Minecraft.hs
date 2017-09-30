{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
module Network.Protocol.Minecraft where

import Control.Lens
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
import System.IO (IOMode(..), hReady)

newtype Minecraft a = Minecraft { unMinecraft :: StateT MinecraftState (EncodedT IO) a
                                } deriving (Functor, Applicative, Monad, MonadIO)

data MinecraftState = MinecraftState { minecraftStateConnectionState :: ConnectionState
                                     , minecraftStateDimension :: Dimension
                                     , minecraftStateDifficulty :: Word8
                                     , minecraftStateGamemode :: Word8
                                     , minecraftStateMc_server :: HostName
                                     , minecraftStateMc_port :: PortNumber
                                     , minecraftStateHandle :: Handle
                                     }
makeFields ''MinecraftState

defaultMinecraftState :: MinecraftState
defaultMinecraftState = MinecraftState { minecraftStateConnectionState = Handshaking
                                       , minecraftStateDimension = Overworld
                                       , minecraftStateDifficulty = 2
                                       , minecraftStateGamemode = 0
                                       , minecraftStateMc_server = ""
                                       , minecraftStateMc_port = 25565
                                       , minecraftStateHandle = undefined
                                       }

runMinecraft :: Handle -> MinecraftState -> Minecraft a -> IO a
runMinecraft handle state = fmap fst . runEncodedT (defaultEncodingState handle) . flip evalStateT state . unMinecraft

getState :: Minecraft MinecraftState
getState = Minecraft get

getStates :: Lens' MinecraftState a -> Minecraft a
getStates = Minecraft . use

getConnectionState :: Minecraft ConnectionState
getConnectionState = Minecraft $ use connectionState

receivePacket :: Minecraft (Maybe CBPacket)
receivePacket = do
    connState <- getConnectionState
    liftMC $ Encoding.readPacket connState

hasPacket :: Minecraft Bool
hasPacket = do
    hdl <- getStates handle
    liftIO $ hReady hdl

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
           hdl <- socketToHandle sock ReadWriteMode
           Right <$> runMinecraft hdl (defaultMinecraftState & mc_server .~ host & mc_port .~ port & handle .~ hdl) mc <* hClose hdl

handshake :: Minecraft ()
handshake = do
    host <- getStates mc_server
    port <- getStates mc_port
    sendPacket $ SBHandshakePayload 335 (NetworkText $ Text.pack host) (fromIntegral port) LoggingIn
    setConnectionState LoggingIn

login :: Text -> Text -> Text -> Minecraft (Either String CBLoginSuccessPayload)
login username uuid token = do
    sendPacket $ SBLoginStartPayload (NetworkText username)
    p <- receivePacket
    case p of
      Just (CBLoginSuccess succ) -> setConnectionState Playing >> pure (Right succ)
      _ -> do
          Just (CBEncryptionRequest encRequest) <- pure p
          sharedSecret <- liftIO $ generateSharedKey
          let serverHash = createServerHash (unNetworkText $ encRequest ^. serverID) sharedSecret ((lengthBS . view pubKey) encRequest)
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
                            Just packet <- receivePacket
                            case packet of
                              CBSetCompression (CBSetCompressionPayload thresh) ->
                                  liftMC $ setCompressionThreshold (fromIntegral thresh) >> pure Nothing
                              CBLoginSuccess x -> setConnectionState Playing >> pure (Just (Right x))
                              _ -> pure . Just $ Left "Unexpected packet received during login"
                        Just (CBJoinGame joinGame) <- receivePacket
                        setGamemode $ joinGame ^. gamemode
                        setDifficulty $ joinGame ^. difficulty
                        setDimension $ joinGame ^. dimension
                        pure loginSuccPacket
    where whileM :: Monad m => m (Maybe a) -> m a
          whileM action = do
              cont <- action
              case cont of
                Nothing -> whileM action
                Just x -> pure x

setConnectionState :: ConnectionState -> Minecraft ()
setConnectionState c = Minecraft $ connectionState .= c

setGamemode :: Word8 -> Minecraft ()
setGamemode gm = Minecraft $ gamemode .= gm

setDifficulty :: Word8 -> Minecraft ()
setDifficulty dif = Minecraft $ difficulty .= dif

setDimension :: Dimension -> Minecraft ()
setDimension dim = Minecraft $ dimension .= dim

liftMC :: EncodedT IO a -> Minecraft a
liftMC = Minecraft . lift
