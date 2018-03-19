{-# LANGUAGE DeriveGeneric, DataKinds, TypeOperators, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, LambdaCase #-}
module Network.Protocol.Minecraft.Yggdrasil ( join
                                            , JoinRequest(..)
                                            , authenticate
                                            , AuthenticationRequest(..)
                                            , AuthenticationResponse(..)
                                            , Agent(..)
                                            , defaultAgent
                                            , Profile(..)
                                            , refresh
                                            , RefreshRequest(..)
                                            , RefreshResponse(..)


                                            , agent
                                            , name
                                            , version
                                            , username
                                            , password
                                            , clientToken
                                            , requestUser
                                            , Network.Protocol.Minecraft.Yggdrasil.id
                                            , legacy
                                            , accessToken
                                            , availableProfiles
                                            , selectedProfile
                                            , serverId
                                            ) where

import Control.Lens
import Data.Aeson
import Data.Char (toLower)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

labelModifier :: Int -> String -> String
labelModifier c = mkLower . drop c
    where mkLower :: String -> String
          mkLower (x:xs) = (toLower x) : xs
          mkLower [] = []

data Agent = Agent { agentName :: Text
                   , agentVersion :: Int
                   } deriving (Generic, Show)

instance ToJSON Agent where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=labelModifier 5}

defaultAgent :: Agent
defaultAgent = Agent "Minecraft" 1

data AuthenticationRequest = AuthenticationRequest { authenticationRequestAgent :: Agent
                                                   , authenticationRequestUsername :: Text
                                                   , authenticationRequestPassword :: Text
                                                   , authenticationRequestClientToken :: Text
                                                   , authenticationRequestRequestUser :: Bool
                                                   } deriving (Generic, Show)

instance ToJSON AuthenticationRequest where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=labelModifier 21}

data Profile = Profile { profileId :: Text
                       , profileName :: Text
                       , profileLegacy :: Maybe Bool
                       } deriving (Generic, Show)

instance FromJSON Profile where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier=labelModifier 7}

instance ToJSON Profile where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=labelModifier 7}

data AuthenticationResponse = AuthenticationResponse { authenticationResponseAccessToken :: Text
                                                     , authenticationResponseClientToken :: Text
                                                     , authenticationResponseAvailableProfiles :: [Profile]
                                                     , authenticationResponseSelectedProfile :: Profile
                                                     } deriving (Generic, Show)
instance FromJSON AuthenticationResponse where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier=labelModifier 22}

data JoinRequest = JoinRequest { joinRequestAccessToken :: Text
                               , joinRequestSelectedProfile :: Text
                               , joinRequestServerId :: Text
                               } deriving (Generic, Show)

instance ToJSON JoinRequest where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=labelModifier 11}

data RefreshRequest = RefreshRequest { refreshRequestAccessToken :: Text
                                     , refreshRequestClientToken :: Text
                                     , refreshRequestRequestUser :: Bool
                                     } deriving (Generic, Show)

instance ToJSON RefreshRequest where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=labelModifier 14}

data RefreshResponse = RefreshResponse { refreshResponseAccessToken :: Text
                                       , refreshResponseClientToken :: Text
                                       , refreshResponseSelectedProfile :: Profile
                                       } deriving (Generic, Show)

instance FromJSON RefreshResponse where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier=labelModifier 15}

makeFields ''Agent
makeFields ''AuthenticationRequest
makeFields ''AuthenticationResponse
makeFields ''Profile
makeFields ''JoinRequest
makeFields ''RefreshRequest
makeFields ''RefreshResponse

--type API = "authenticate" :> ReqBody '[JSON] AuthenticationRequest

type SessionAPI = "session" :> "minecraft" :> "join" :> ReqBody '[JSON] JoinRequest :> PostNoContent '[JSON] NoContent

type AuthAPI = "authenticate" :> ReqBody '[JSON] AuthenticationRequest :> Post '[JSON] AuthenticationResponse
          :<|> "refresh" :> ReqBody '[JSON] RefreshRequest :> Post '[JSON] RefreshResponse

sessionApi :: Proxy SessionAPI
sessionApi = Proxy

authApi :: Proxy AuthAPI
authApi = Proxy


execute :: String -> ClientM a -> IO (Either ServantError a)
execute uri client = do
    manager <- newManager tlsManagerSettings
    runClientM client $ ClientEnv manager (BaseUrl Https uri 443 "")

executeAuth :: ClientM a -> IO (Either ServantError a)
executeAuth = execute "authserver.mojang.com"

executeSession :: ClientM a -> IO (Either ServantError a)
executeSession = execute "sessionserver.mojang.com"


joinM :: JoinRequest -> ClientM NoContent
joinM = client sessionApi

join :: JoinRequest -> IO Bool
join req = isRight <$> executeSession (joinM req)

authenticateM :: AuthenticationRequest -> ClientM AuthenticationResponse
refreshM :: RefreshRequest -> ClientM RefreshResponse
authenticateM :<|> refreshM = client authApi

authenticate :: AuthenticationRequest -> IO (Maybe AuthenticationResponse)
authenticate req = executeAuth (authenticateM req) >>= \case
    Left err -> print err >> pure Nothing
    Right res -> pure $ Just res

refresh :: RefreshRequest -> IO (Maybe RefreshResponse)
refresh req = executeAuth (refreshM req) >>= \case
    Left err -> print err >> pure Nothing
    Right res -> pure $ Just res
