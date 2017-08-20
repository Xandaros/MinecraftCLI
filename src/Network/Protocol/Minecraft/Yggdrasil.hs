{-# LANGUAGE DeriveGeneric, DataKinds, TypeOperators #-}
module Network.Protocol.Minecraft.Yggdrasil ( join
                                            , JoinRequest(..)
                                            ) where

import Data.Aeson
import Data.Either (isRight)
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

--data Agent = Agent { name :: Text
--                   , version :: Int
--                   } deriving (Generic, Show)
--
--data AuthenticationRequest = AuthenticationRequest { agent :: Agent
--                                                   , username :: Text
--                                                   , password :: Text
--                                                   , clientToken :: Text
--                                                   , requestUser :: Bool
--                                                   } deriving (Generic, Show)

data JoinRequest = JoinRequest { accessToken :: Text
                               , selectedProfile :: Text
                               , serverId :: Text
                               } deriving (Generic, Show)

instance ToJSON JoinRequest

--type API = "authenticate" :> ReqBody '[JSON] AuthenticationRequest

type API = "session" :> "minecraft" :> "join" :> ReqBody '[JSON] JoinRequest :> PostNoContent '[JSON] NoContent

api :: Proxy API
api = Proxy

joinM :: JoinRequest -> ClientM NoContent
joinM = client api

join :: JoinRequest -> IO Bool
join req = do
    manager <- newManager tlsManagerSettings
    res <- runClientM (joinM req) $ ClientEnv manager (BaseUrl Https "sessionserver.mojang.com" 443 "")
    case res of
      Left err -> pure ()
      Right _ -> pure ()
    pure $ isRight res
