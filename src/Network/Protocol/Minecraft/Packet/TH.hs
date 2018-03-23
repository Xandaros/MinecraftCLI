{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Network.Protocol.Minecraft.Packet.TH ( packets
                                            ) where

import Control.Lens.TH
import Control.Monad (join)
import Data.Binary (Binary, put, get, Get)
import Data.Char (toLower, toUpper)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parsec

import Network.Protocol.Minecraft.Types

type Parser = Parsec String ()

data Declaration = Declaration { declarationName :: String
                               , connectionState :: ConnectionState
                               , packetID :: Integer
                               , fields :: [Field]
                               , declarationDerives :: [String]
                               , declarationInstances :: [String]
                               } deriving (Show)

data Field = Field { fieldName :: String
                   , fieldType :: String
                   } deriving (Show)

parseDeclarations :: Monad m => (String, Int, Int) -> String -> m ([Declaration], [Declaration])
parseDeclarations (file, line, col) s =
    case parse p "" s of
      Left err -> fail $ show err
      Right decls -> pure decls
    where
        p :: Parser ([Declaration], [Declaration])
        p = do
            pos <- getPosition
            setPosition $ (flip setSourceName) file $ (flip setSourceLine) line $ (flip setSourceColumn) col $ pos
            spaces
            _ <- string "[Clientbound]"
            spaces
            cb <- many $ (parseDeclaration <* many newline)
            spaces
            _ <- string "[Serverbound]"
            spaces
            sb <- many $ (parseDeclaration <* many newline)
            pure (cb, sb)

parseDeclaration :: Parser Declaration
parseDeclaration = Declaration <$> many alphaNum
                               <*> (spaces *> parseConnectionState)
                               <*> (spaces *> integer)
                               <*> many (try (newline *> parseField))
                               <*> (try $ newline *> parseKWList "deriving")
                               <*> (newline *> parseKWList "instance")

integer :: Parser Integer
integer = read <$> (hexadecimal <|> decimal)
    where decimal = many digit
          hexadecimal = do
              _ <- try $ string "0x"
              digits <- many1 hexDigit
              pure $ "0x" ++ digits

parseConnectionState :: Parser ConnectionState
parseConnectionState = const Playing <$> string "Playing"
                   <|> const LoggingIn <$> string "LoggingIn"
                   <|> const Handshaking <$> string "Handshaking"
                   <|> const GettingStatus <$> string "GettingStatus"

parseField :: Parser Field
parseField = Field <$> (space *> spaces *> many alphaNum)
                   <*> (spaces *> string "::" *> spaces *> many alphaNum)

parseKWList :: String -> Parser [String]
parseKWList kw = space *> spaces *> string kw *> spaces *> char '(' *>
    ((spaces *> many1 alphaNum <* spaces) `sepBy` char ',') <*
    char ')'

packets :: QuasiQuoter
packets = QuasiQuoter { quoteDec = declarationsQuoter
                      , quoteExp = undefined
                      , quotePat = undefined
                      , quoteType = undefined
                      }

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

mkBangType :: Type -> BangType
mkBangType t = (defaultBang, t)

mkVarBangType :: Name -> Type -> VarBangType
mkVarBangType n t = (n, defaultBang, t)

mkType :: [String] -> Q Type
mkType [] = fail "This should never happen"
mkType (x:xs) = do
    Just typ <- lookupTypeName x
    if null xs
       then pure $ ConT typ
       else AppT (ConT typ) <$> mkType xs

declarationsQuoter :: String -> DecsQ
declarationsQuoter s = do
    loc <- location
    (cb, sb) <- parseDeclarations (loc_filename loc, fst $ loc_start loc, snd $ loc_start loc) s
    (cbdecs, cbpayloads) <- mkDecs "CB" cb
    (sbdecs, sbpayloads) <- mkDecs "SB" sb
    lensifiedPayloads <- declareFields (pure $ cbpayloads ++ sbpayloads)
    pure $ getPacketSig : mkGetPacket cb : cbdecs ++ sbdecs ++ lensifiedPayloads


mkDecs :: String -> [Declaration] -> Q ([Dec], [Dec])
mkDecs prefix decls = do
    (cons, payloads) <- unzip <$> (sequence $ mkConsPayload True prefix <$> decls)
    (unknownC, unknownPL) <- mkConsPayload False prefix $ Declaration { declarationName = "Unknown"
                                                                      , fields = [Field "unknownPayload" "ByteString"]
                                                                      , declarationDerives = ["Show"]
                                                                      , declarationInstances = []
                                                                      , connectionState = undefined
                                                                      , packetID = undefined
                                                                      }

    let packetName = mkName $ prefix ++ "Packet"
        packet = DataD [] packetName [] Nothing (unknownC : cons) [DerivClause Nothing [ConT ''Show, ConT ''Generic]]
    packetHasPacketID <- generateHasPacketID packetName cons
    packetBinaryInstance <- packetBinary packetName cons
    pure $ (packet : packetHasPacketID : [packetBinaryInstance], unknownPL ++ join payloads)

packetBinary :: Name -> [Con] -> Q Dec
packetBinary packet cons = do
    let cons' = conName <$> cons
    putDef' <- putDef cons'
    pure $ InstanceD Nothing [] (AppT (ConT ''Binary) (ConT packet)) [putDef']
    where putDef :: [Name] -> Q Dec
          putDef cons = do
              pats <- sequenceQ $ do
                  con <- cons
                  pure $ do
                      varp <- newName "a"
                      pure $ Clause [ConP con [VarP varp]] (NormalB (AppE (VarE 'put) (VarE varp))) []
              pure $ FunD 'put pats
          conName :: Con -> Name
          conName (NormalC name _) = name
          conName _ = error "This should never happen"

generateHasPacketID :: Name -> [Con] -> Q Dec
generateHasPacketID packetName cons = do
    let passthrough name = do
            (NormalC con _ ) <- cons
            pure $ do
                pat <- newName "x"
                pure $ Clause [ConP con [VarP pat]] (NormalB $ AppE (VarE name) (VarE pat)) []
    packetIDDecs <- sequence $ passthrough 'getPacketID
    modeDecs <- sequence $ passthrough 'mode
    let header = InstanceD Nothing [] (AppT (ConT ''HasPacketID) (ConT packetName))
                   ([ FunD 'getPacketID packetIDDecs
                    , FunD 'mode modeDecs
                    ])
    pure header

mkConsPayload :: Bool -> String -> Declaration -> Q (Con, [Dec])
mkConsPayload hasPacketID prefix Declaration{..} = do
    let payloadType = mkName $ prefix ++ declarationName ++ "Payload"
        fieldPrefix = [toLower (head prefix)] ++ drop 1 prefix ++ declarationName ++ "Payload"
        payloadCon Field{..} = mkVarBangType (mkName $ fieldPrefix ++ [toUpper (head fieldName)] ++ drop 1 fieldName) <$> mkType (words fieldType)
    payloadCons <- RecC payloadType <$> sequence (payloadCon <$> fields)
    derives <- DerivClause Nothing <$> sequence (mkType . words <$> declarationDerives)
    let payload = DataD [] payloadType [] Nothing [payloadCons] [derives]

    instances <- sequence $ mkType . words <$> declarationInstances
    let instanceDecls = flip (InstanceD Nothing []) [] <$> (flip AppT (ConT payloadType) <$> (instances))

    packetIDInstance <-
        if hasPacketID
           then do
                mode <- lift connectionState
                let packetIDFun = FunD (mkName "getPacketID") [Clause [VarP (mkName "_")] (NormalB $ LitE $ IntegerL $ fromIntegral packetID) []]
                    modeFun = FunD (mkName "mode") [Clause [VarP (mkName "_")] (NormalB mode) []]
                pure $ [InstanceD Nothing [] (AppT (ConT ''HasPacketID) (ConT payloadType)) [packetIDFun, modeFun]]
            else pure []

    let con = NormalC (mkName $ prefix ++ declarationName) . (:[]) . mkBangType $ ConT payloadType
    pure $ (con, payload : packetIDInstance ++ instanceDecls)

getPacketSig :: Dec
getPacketSig = SigD (mkName "getPacket") (AppT (AppT ArrowT (ConT ''ConnectionState)) (AppT (ConT ''Get) (ConT (mkName "CBPacket"))))

mkGetPacket :: [Declaration] -> Dec
mkGetPacket decls = FunD (mkName "getPacket") $ clauses
    where hanshakingDecls = filter ((==Handshaking) . connectionState) decls
          loggingInDecls = filter ((==LoggingIn) . connectionState) decls
          playingDecls = filter ((==Playing) . connectionState) decls
          clauses = --mkGetPacketClause 'Handshaking hanshakingDecls
                    mkGetPacketClause 'LoggingIn loggingInDecls
                  : mkGetPacketClause 'Playing playingDecls
                  : [Clause [WildP] defaultBody []]

          mkGetPacketClause :: Name -> [Declaration] -> Clause
          mkGetPacketClause state decls = Clause [ConP state []]
                                                 (NormalB (DoE [ BindS (VarP (mkName "pid")) (SigE (VarE 'get) (AppT (ConT ''Get) (ConT ''VarInt)))
                                                               , NoBindS (CaseE (VarE (mkName "pid")) $ (mkGetPacketMatch <$> decls) ++ [Match WildP defaultBody []])
                                                               ]))
                                                 []
          mkGetPacketMatch :: Declaration -> Match
          mkGetPacketMatch Declaration{..} = Match (LitP (IntegerL packetID)) (NormalB (AppE (AppE (VarE 'fmap) (ConE (mkName $ "CB" ++ declarationName))) (VarE 'get))) []

          defaultBody :: Body
          defaultBody = NormalB (AppE (AppE (VarE 'fmap) (ConE (mkName "CBUnknown"))) (VarE 'get))
