{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB ( getProfiles
          , newProfile
          , deleteProfile
          , getProfile
          , updateAccessToken
          , getClientToken
          , getServers
          , newServer
          , deleteServer
          , getServer
          , Profile(..)
          , profileUsername
          , profileUuid
          , profileToken
          , Server(..)
          , serverName
          , serverAddress
          ) where
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [ mkPersist sqlSettings {mpsGenerateLenses = True}
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
Profile
    username Text
    uuid Text
    token Text
    UniqueProfileUsername username
    deriving Show
KV
    key Text
    value Text
    UniqueKey key
    deriving Show
Server
    name Text
    address Text
    UniqueServerName name
    deriving Show
|]

database :: Text
database = "MinecraftCLI.db"

getClientToken :: IO Text
getClientToken = runSqlite database $ do
    runMigration migrateAll
    tokenm <- getBy (UniqueKey "clientToken")
    case tokenm of
      Just token -> pure $ entityVal token ^. kVValue
      Nothing -> do
          token <- UUID.toText <$> liftIO UUID.nextRandom
          _ <- insert (KV "clientToken" token)
          pure token

getProfiles :: IO [Profile]
getProfiles = runSqlite database $ do
    runMigration migrateAll
    fmap entityVal <$> selectList [] []

newProfile :: Profile -> IO ()
newProfile prof = runSqlite database $ do
    runMigration migrateAll
    void $ insert prof

deleteProfile :: Text -> IO ()
deleteProfile username = runSqlite database $ do
    runMigration migrateAll
    deleteBy (UniqueProfileUsername username)

getProfile :: Text -> IO (Maybe Profile)
getProfile username = runSqlite database $ do
    runMigration migrateAll
    fmap entityVal <$> getBy (UniqueProfileUsername username)

updateAccessToken :: Text -> Text -> IO ()
updateAccessToken username accessToken = runSqlite database $ do
    runMigration migrateAll
    updateWhere [ProfileUsername ==. username] [ProfileToken =. accessToken]

getServers :: IO [Server]
getServers = runSqlite database $ do
    runMigration migrateAll
    fmap entityVal <$> selectList [] []

newServer :: Server -> IO ()
newServer server = runSqlite database $ do
    runMigration migrateAll
    void $ insert server

deleteServer :: Text -> IO ()
deleteServer name = runSqlite database $ do
    runMigration migrateAll
    deleteBy (UniqueServerName name)

getServer :: Text -> IO (Maybe Server)
getServer name = runSqlite database $ do
    runMigration migrateAll
    fmap entityVal <$> getBy (UniqueServerName name)
