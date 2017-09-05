{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Network.Protocol.Minecraft.Packet.TH ( packetsCB
                                            , packetsSB
                                            , lensify
                                            ) where

import Control.Lens.TH
import Control.Monad (join)
import Data.Char (toLower, toUpper)
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

parseDeclarations :: Monad m => (String, Int, Int) -> String -> m [Declaration]
parseDeclarations (file, line, col) s =
    case parse p "" s of
      Left err -> fail $ show err
      Right decls -> pure decls
    where
        p :: Parser [Declaration]
        p = do
            pos <- getPosition
            setPosition $ (flip setSourceName) file $ (flip setSourceLine) line $ (flip setSourceColumn) col $ pos
            spaces
            many $ (parseDeclaration <* many newline)

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

packetsCB :: QuasiQuoter
packetsCB = QuasiQuoter { quoteDec = declarationsQuoter "CB"
                        , quoteExp = undefined
                        , quotePat = undefined
                        , quoteType = undefined
                        }

packetsSB :: QuasiQuoter
packetsSB = QuasiQuoter { quoteDec = declarationsQuoter "SB"
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

declarationsQuoter :: String -> String -> DecsQ
declarationsQuoter prefix s = do
    loc <- location
    decls <- parseDeclarations (loc_filename loc, fst $ loc_start loc, snd $ loc_start loc) s

    (cons, payloads) <- unzip <$> (sequence $ mkConsPayload True prefix <$> decls)
    (unknownC, unknownPL) <- mkConsPayload False prefix $ Declaration { declarationName = "Unknown"
                                                                      , fields = [Field "unknownPayload" "ByteString"]
                                                                      , declarationDerives = ["Show"]
                                                                      , declarationInstances = []
                                                                      , connectionState = undefined
                                                                      , packetID = undefined
                                                                      }

    let packet = DataD [] (mkName $ prefix ++ "Packet") [] Nothing (unknownC : cons) [DerivClause Nothing [ConT ''Show]]
    pure $ packet : join payloads ++ unknownPL

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

lensify :: Name -> DecsQ
lensify packet' = do
    TyConI (DataD _ _ _ _ cons _) <- reify packet'
    let conNames = (\(NormalC _ [(_, ConT n)]) -> n) <$> cons
    join <$> makeFields `mapM` conNames
