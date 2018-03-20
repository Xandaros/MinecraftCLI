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
import           Control.Monad (void, when, forever, forM_)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Safe
import           System.Console.Haskeline
import           System.Exit (exitSuccess)
import           Reflex hiding (performEvent_)
import           Reflex.Host.App

import Network.Protocol.Minecraft
import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types
import qualified Network.Protocol.Minecraft.Yggdrasil as Yggdrasil
import Network.Protocol.Minecraft.Yggdrasil ()

import Commands
import DB ( Profile(..), profileUsername, profileUuid, profileToken
          , Server(..), serverName, serverAddress
          )
import qualified DB
import Inventory

import Debug.Trace

minecraftThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> Server -> (String -> IO ()) -> IO ()
minecraftThread inbound outbound shutdown profile server printfunc = do
    void $ connect (T.unpack $ server ^. serverAddress) Nothing $ do
        liftIO $ printfunc "Sending handshake"
        handshake
        liftIO $ printfunc "Handshake sent"

        loginSucc <- login (profile ^. profileUsername) (profile ^. profileUuid) (profile ^. profileToken)

        case loginSucc of
          Left err -> liftIO $ putStrLn err
          Right _ -> liftIO $ do
              printfunc "Logged in"

        sendPacket $ SBClientSettingsPayload "en_GB" 1 0 True 0x7F 1
        sendPacket $ SBClientStatusPayload 0
        sendPacket $ SBChatMessage $ SBChatMessagePayload ("Beep. Boop. I'm a bot. Type \"" <> profile ^. profileUsername . network <> ": help\" for more information")
        sendPacket $ SBChatMessagePayload "/afk"

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
                          liftIO $ printfunc "Position and look"
                          sendPacket $ SBTeleportConfirmPayload $ positionAndLook ^. posLookID
                          sendPacket $ SBPlayerPositionAndLookPayload (positionAndLook^.x) (positionAndLook^.y) (positionAndLook^.z) (positionAndLook^.yaw) (positionAndLook^.pitch) True
                          liftIO . atomically $ writeTChan inbound packet
                      CBDisconnectPlay dc -> do
                          liftIO $ printfunc $ "Disconnected: " ++ T.unpack (dc ^. reason . from network)
                      _ -> liftIO . atomically $ writeTChan inbound packet
                  Nothing -> liftIO $ do
                      writeIORef shutdown True
                      printfunc "Connection closed" >> exitSuccess
            dataToSend <- liftIO . atomically $ tryReadTChan outbound
            case dataToSend of
              Just dataToSend -> sequence_ $ sendPacket <$> dataToSend
              Nothing -> pure ()
            liftIO $ when (not packetAvailable) $ do
                threadDelay (floor $ ((1/21) :: Double) * 1000000)


frpThread :: TChan CBPacket -> TChan [SBPacket] -> IORef Bool -> Profile -> (String -> IO ()) -> IO ()
frpThread inbound outbound shutdown profile printfunc = runSpiderHost $ hostApp app
    where app :: MonadAppHost t m => m ()
          app = do
              (inputEvent, inputFire) <- newExternalEvent
              (tickEvent, tickFire) <- newExternalEvent
              void . liftIO . forkIO . forever $ threadDelay (floor (1/20 * 1000000 :: Double)) >>= tickFire
              void . liftIO . forkIO . forever $ atomically (readTChan inbound) >>= inputFire
              (outEvent, printEvent, shutdownEvent) <- minecraftBot (profile ^. profileUsername) inputEvent tickEvent
              performEvent_ $ fmap (liftIO . atomically . writeTChan outbound) outEvent
              performEvent_ $ fmap (sequence_ . fmap liftIO . fmap (printfunc . T.unpack) . NE.toList) printEvent
              performEvent_ $ fmap (const . liftIO $ threadDelay 10000 >> writeIORef shutdown True >> exitSuccess) shutdownEvent
              pure ()

minecraftBot :: (MonadHold t m, MonadFix m, Reflex t) => Text -> Event t CBPacket -> Event t ()
             -> m (Event t [SBPacket], Event t (NonEmpty Text), Event t ())
minecraftBot botName inbound tick = do
    let chatMessages = chatMessageE inbound
        commands = commandE botName chatMessages
    -- ticks <- zipListWithEvent (\a _ -> a) [1..] tick
    -- seconds <- zipListWithEvent (\a _ -> a) ([1..] :: [Integer]) $ ffilter ((==0) . (`mod` (20 :: Integer))) ticks
    playerPos <- playerPositionD inbound (tpCommandE commands)
    let inventoryActions  = mergeList [dropCommandE commands]
    (transactionPackets, localInventoryActions) <- transactionsD inventoryActions (transactionConfirmationsE inbound)
    inventory <- inventoryD (leftmost $ [(cbInventoryActionsE inbound), localInventoryActions])
    pure ( foldl1 (<>) [ NE.toList <$> (fmap (SBChatMessage . SBChatMessagePayload . view network) <$> commandMessagesE commands)
                       , transactionPackets
                       , NE.toList <$> (mergeList [ whereCommandE commands playerPos
                                                  , mkPPAL <$> tag (current playerPos) tick
                                                  , inventoryCommandE commands inventory
                                                  --, chatString . show <$> tagPromptlyDyn transactions (ffilter ((==0) . (`mod` 10)) seconds)
                                                  ])
                       ]
         , mergeList [ T.pack . show <$> chatMessages
                     , T.pack . show <$> commands
                     , windowItemsE inbound
                     , T.pack . show <$> transactionConfirmationsE inbound
                     ]
         , void $ quitCommandE commands
         )

windowItemsE :: Reflex t => Event t CBPacket -> Event t Text
windowItemsE inbound = fforMaybe inbound $ \case
    CBWindowItems (CBWindowItemsPayload wid slot) -> Just $ T.pack $ "WindowItems " <> show (wid, slot)
    CBSetSlot (CBSetSlotPayload wid slot slotData) -> Just $ T.pack $ "SetSlot " <> show (wid, slot, slotData)
    _ -> Nothing

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

commandE :: Reflex t => Text -> Event t ChatMsg -> Event t ChatCommand
commandE botName = fmapMaybe (chatToCommand botName)

chatToString :: Text -> Maybe String
chatToString c = T.unpack . chatToText . canonicalizeChatComponent <$> decode (BSL.fromStrict $ TE.encodeUtf8 c)

chatToCommand :: Text -> ChatMsg -> Maybe ChatCommand
chatToCommand botName msg = do
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

profileInput :: [String] -> InputT IO ()
profileInput ("help":_) = do
    outputStrLn "Available subcommands:"
    outputStrLn "help              - Display this help"
    outputStrLn "list              - List available profiles"
    outputStrLn "new <login>       - Create a new profile"
    outputStrLn "delete <username> - Delete an existing profile"
profileInput ("list":_) = do
    profiles <- liftIO DB.getProfiles
    forM_ profiles $ \prof -> do
        outputStrLn . T.unpack $ prof ^. profileUsername
profileInput ("new":login:_) = do
    passwordm <- getPassword Nothing "Password: "
    case passwordm of
        Nothing -> pure ()
        Just password -> do
            clientToken <- liftIO DB.getClientToken
            responsem <- liftIO $ Yggdrasil.authenticate (Yggdrasil.AuthenticationRequest Yggdrasil.defaultAgent
                                                                                          (T.pack login)
                                                                                          (T.pack $ password)
                                                                                          clientToken
                                                                                          False)
            case responsem of
                Nothing -> outputStrLn "Unable to create profile (wrong login/password?)"
                Just response -> do
                    let profile = response ^. Yggdrasil.selectedProfile
                        accessToken = response ^. Yggdrasil.accessToken :: Text
                    liftIO $ DB.newProfile (Profile (profile ^. Yggdrasil.name) (profile ^. Yggdrasil.id) accessToken)
                    outputStrLn "Profile created"
profileInput ("delete":username:_) = do
    liftIO $ DB.deleteProfile (T.pack username)
    outputStrLn "Profile deleted"
profileInput _ = profileInput ["help"]

serverInput :: [String] -> InputT IO ()
serverInput ("help":_) = do
    outputStrLn "Available subcommands:"
    outputStrLn "help                 - Display this help"
    outputStrLn "list                 - List available servers"
    outputStrLn "new <name> <address> - Create a new server"
    outputStrLn "delete <name>        - Delete an existing server"
serverInput ("list":_) = do
    servers <- liftIO DB.getServers
    forM_ servers $ \server -> do
        outputStrLn . T.unpack $ server ^. serverName
serverInput ("new":name:address:_) = do
    liftIO $ DB.newServer (Server (T.pack name) (T.pack address))
    outputStrLn "Server created"
serverInput ("delete":name:_) = do
    liftIO $ DB.deleteServer (T.pack name)
    outputStrLn "Server deleted"
serverInput _ = serverInput ["help"]

connectInput :: [String] -> InputT IO ()
connectInput (profile:server:_) = do
    outputStrLn "Connecting to server"
    printfunc <- getExternalPrint
    liftIO $ do
        profilem <- DB.getProfile (T.pack profile)
        serverm <- DB.getServer (T.pack server)
        case profilem of
          Nothing -> pure ()
          Just profile ->
              case serverm of
                Nothing -> pure ()
                Just server -> do
                    clientToken <- DB.getClientToken
                    responsem <- Yggdrasil.refresh (Yggdrasil.RefreshRequest (profile ^. profileToken)
                                                                             clientToken
                                                                             False)
                    case responsem of
                      Nothing -> printfunc "Could not refresh token - try recreating the profile"
                      Just response -> do
                          DB.updateAccessToken (profile ^. profileUsername) (response ^. Yggdrasil.accessToken)
                          let newprofile = profile & profileToken .~ (response ^. Yggdrasil.accessToken)
                          inbound <- atomically $ newTChan
                          outbound <- atomically $ newTChan
                          shutdown <- newIORef False
                          async (minecraftThread inbound outbound shutdown newprofile server printfunc) >>= link
                          async (frpThread inbound outbound shutdown newprofile printfunc) >>= link
connectInput _ = outputStrLn "You need to provide a profile and a server"

main :: IO ()
main = do
    runInputT defaultSettings $ do
        loop
        where loop :: InputT IO ()
              loop = do
                  minput <- getInputLine "% "
                  case words <$> minput of
                    Nothing -> pure ()
                    Just ("quit":_) -> pure ()
                    Just ("profiles":args) -> profileInput args >> loop
                    Just ("servers":args) -> serverInput args >> loop
                    Just ("connect":args) -> connectInput args >> loop
                    Just input -> do
                        outputStrLn ("Unrecognised command: " ++ unwords input)
                        outputStrLn ("Available commands:")
                        outputStrLn ("quit                       - Close MinecraftCLI")
                        outputStrLn ("profiles                   - Manage profiles")
                        outputStrLn ("servers                    - Manage servers")
                        outputStrLn ("connect <profile> <server> - Connect a profile to a server")
                        loop

mkPPAL :: (Double, Double, Double) -> SBPacket
mkPPAL (x, y, z) = SBPlayerPositionAndLook $ SBPlayerPositionAndLookPayload (x ^. network) (y ^. network) (z ^. network) 0 0 True

whileM :: Monad f => f Bool -> f () -> f ()
whileM c x = c >>= \case
    False -> pure ()
    True -> x >> whileM c x

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
