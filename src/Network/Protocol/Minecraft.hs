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

newtype MinecraftT m a = MinecraftT { unMinecraftT :: StateT MinecraftState (EncodedT m) a
                                    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans MinecraftT where
    lift = MinecraftT . lift . lift

type Minecraft = MinecraftT IO

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

runMinecraftT :: Monad m => Handle -> MinecraftState -> MinecraftT m a -> m a
runMinecraftT handle state = fmap fst . runEncodedT (defaultEncodingState handle) . flip evalStateT state . unMinecraftT

runMinecraft :: Handle -> MinecraftState -> Minecraft a -> IO a
runMinecraft = runMinecraftT

getState :: Monad m => MinecraftT m MinecraftState
getState = MinecraftT get

getStates :: Monad m => Lens' MinecraftState a -> MinecraftT m a
getStates = MinecraftT . use

getConnectionState :: Monad m => MinecraftT m ConnectionState
getConnectionState = MinecraftT $ use connectionState

receivePacket :: (Monad m, MonadIO m) => MinecraftT m (Maybe CBPacket)
receivePacket = do
    connState <- getConnectionState
    liftMC $ Encoding.readPacket connState

hasPacket :: (Monad m, MonadIO m) => MinecraftT m Bool
hasPacket = getStates handle >>= liftIO . hReady

sendPacket :: (HasPacketID a, Binary a, Monad m, MonadIO m) => a -> MinecraftT m ()
sendPacket p = liftMC $ Encoding.sendPacket p

connect :: (Monad m, MonadIO m) => HostName -> Maybe PortNumber -> MinecraftT m a -> m (Either String a)
connect host port' mc = do
    let port = fromMaybe 25565 port'
    addrs <- liftIO $ getAddrInfo Nothing (Just host) (Just $ show port)
    if null addrs
       then pure . Left $ "Unable to find host " ++ host
       else do
           sock <- liftIO $ socket AF_INET Stream defaultProtocol
           liftIO $ print (addrAddress $ addrs !! 0)
           liftIO $ Socket.connect sock (addrAddress $ addrs !! 0)
           hdl <- liftIO $ socketToHandle sock ReadWriteMode
           Right <$> runMinecraftT hdl (defaultMinecraftState & mc_server .~ host & mc_port .~ port & handle .~ hdl) mc
                  <* liftIO (hClose hdl)

handshake :: (Monad m, MonadIO m) => MinecraftT m ()
handshake = do
    host <- getStates mc_server
    port <- getStates mc_port
    sendPacket $ SBHandshakePayload 340 (NetworkText $ Text.pack host) (fromIntegral port) LoggingIn
    setConnectionState LoggingIn

login :: (Monad m, MonadIO m) => Text -> Text -> Text -> MinecraftT m (Either String CBLoginSuccessPayload)
login username uuid token = do
    sendPacket $ SBLoginStartPayload (NetworkText username)
    p <- receivePacket
    case p of
      Just (CBLoginSuccess succ) -> setConnectionState Playing >> pure (Right succ)
      Just (CBEncryptionRequest encRequest) -> do
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

setConnectionState :: Monad m => ConnectionState -> MinecraftT m ()
setConnectionState c = MinecraftT $ connectionState .= c

setGamemode :: Monad m => Word8 -> MinecraftT m ()
setGamemode gm = MinecraftT $ gamemode .= gm

setDifficulty :: Monad m => Word8 -> MinecraftT m ()
setDifficulty dif = MinecraftT $ difficulty .= dif

setDimension :: Monad m => Dimension -> MinecraftT m ()
setDimension dim = MinecraftT $ dimension .= dim

liftMC :: Monad m => EncodedT m a -> MinecraftT m a
liftMC = MinecraftT . lift
