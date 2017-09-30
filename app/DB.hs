{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB where
import Data.Text (Text)
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
    active Bool
|]

getActiveProfiles :: IO [Profile]
getActiveProfiles = runSqlite "MinecraftCLI.db" $ do
    runMigration migrateAll

    fmap entityVal <$> selectList [ProfileActive ==. True] []
