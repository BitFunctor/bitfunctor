{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StatementExtraction where

import Data.Text as DT
import Data.Binary
import Data.ByteArray
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson
import Data.ByteArray (convert)
import GHC.Generics
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE
import qualified Crypto.Hash as H (hash, digestFromByteString)
import Crypto.Hash.Algorithms (HashAlgorithm, Keccak_256)
import Crypto.Hash (Digest)
import qualified Data.Text.Encoding as TE
import Data.Binary as Binary (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)

{-- imported from other sources --}

import System.Process
import GHC.IO.Handle
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec.Number (decimal, int, nat)
import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)
import qualified Data.List as DL (map, nub)
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified Data.ByteString as DBS (hGet)

data HashAlgorithm a =>
     Hash a = Hash (Digest a)
              deriving (Eq, Ord, Show)

instance (HashAlgorithm a) => ToJSON (Hash a) where
  toJSON (Hash d) = String . TE.decodeUtf8 . B16.encode $ convert d


type SecretKey = Ed25519.SecretKey
type PublicKey = Ed25519.PublicKey
type Signature = Ed25519.Signature


instance ToJSON PublicKey where
  toJSON = String . TE.decodeUtf8 . B16.encode . convert

instance ToJSON Signature where
  toJSON = String . TE.decodeUtf8 . B16.encode . convert


type Id = Keccak_256

hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Hash a
hash = Hash . H.hash

data Code = CoqText Text
            deriving (Eq, Show, Generic)

data Kind = Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof | Library | Module | Section | Inductive
            deriving (Eq, Show, Generic)

instance Binary Text where
  put = put . DT.unpack
  get = get >>= return . DT.pack

instance Binary Code where
  put (CoqText a) = put a
  get = get >>= \a -> return (CoqText a)

instance Binary Kind where
  put Definition = putWord8 0
  put Theorem = putWord8 1
  put Notation = putWord8 2
  put Tactic = putWord8 3
  put Variable = putWord8 4
  put Constructor = putWord8 5
  put Proof = putWord8 6
  put Library = putWord8 7
  put Module = putWord8 8
  put Section = putWord8 9
  put Inductive = putWord8 10
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Definition
      1 -> return Theorem
      2 -> return Notation
      3 -> return Tactic
      4 -> return Variable
      5 -> return Constructor
      6 -> return Proof
      7 -> return Library
      8 -> return Module
      9 -> return Section
      10 -> return Inductive
      _ -> fail "Binary_Kind_get: Kind cannot be parsed"

instance Binary Id where
{--  put _ = putWord8 0
  get  = do
    tag_ <- getWord8
    case tag_ of
      0 -> return undefined
      _ -> fail "no parse" --}

instance Binary (Digest Id)
 
instance Binary (Hash Id) where
   put (Hash d) = put . B16.encode $ convert d
   get = get >>= \a -> return (Hash a)

{----------------------------------------------------------}

class Identifiable a where
  id :: a -> Hash Id

data StatementA a = Statement { name :: Text
                           , kind :: Kind
                           , code :: Code
                           , uses :: [a]
                           } deriving (Eq, Show, Generic)

type Statement = StatementA (Hash Id)

instance Binary Statement where
  put s = do
           put (name s)
           put (kind s)
           put (code s)
           put (uses s)
  get = do n <- get
           k <- get
           c <- get
           u <- get
           return $ Statement n k c u

instance Identifiable Statement where
  id = hash . toStrict . Binary.encode

instance Identifiable Kind where
  id = hash . toStrict . Binary.encode


type GlobFileDigest = String

type GlobFileName = String

{--
data GlobFileImport = GlobFileImport {ispos::Int,
                                      iepos::Int,
                                      ilibname::String} deriving (Eq, Show, Generic)
--}

data GlobFileRawEntry = GlobFileRawEntry {espos:: Int,
                                    eepos:: Int,
                                    ekind:: Kind,
                                    elibname:: String,
                                    emodname:: String,
                                    ename:: String} deriving (Eq, Show, Generic)

data GlobFileEntry = GlobFileResource GlobFileRawEntry | GlobFileStatement GlobFileRawEntry
                     deriving (Eq, Show, Generic)

type GlobFileData = (GlobFileDigest, GlobFileName, [GlobFileEntry])

globfileData :: Parser GlobFileData
globfileData = do
                dig <- globfileDigest
                newline
                name <- globfileName
                newline
                sts <- many (globfileStatement <|> globfileResource)
                return (dig, name, sts)


globfileDigest = string "DIGEST" >> spaces >> globfileIdent 
globfileName = char 'F' >> globfileIdent

{--globfileImport = do
                   char 'R'
                   sbyte <- decimal
                   char ':'
                   ebyte <- decimal
                   spaces
                   name <- globfileIdent
                   spaces
                   string "<>"
                   spaces
                   string "<>"
                   spaces
                   string "lib"
                   newline
                   return $ GlobFileImport sbyte ebyte name
--}

-- TODO: Compare with Coq correct idents
-- .<>[]'_,:=/\\

globfileIdent = do
                 i <-  many1 (letter <|> digit <|> oneOf "._")
                       <|> do {string "<>" ; return ""}
                 return $ trim i

globfileNot = many1 (letter <|> digit <|> oneOf ".<>[]'_,:=/\\") >>= return . trim

trim = f . f
       where f = Prelude.reverse . Prelude.dropWhile isSpace


globfileStatement = do
                     kind <-      do {string "def"; return Definition}
                              <|> do {string "not"; return Notation}
                              <|> do {string "ind"; return Inductive}
                              <|> do {string "constr"; return Constructor}
                              <|> do {string "prf"; return Proof}
                              <|> do {string "mod"; return Module}
                              <|> do {string "sec"; return Section}
                              <|> do {string "var"; return Variable}
                     spaces
                     sbyte <- decimal
                     mebyte <- optionMaybe (char ':' >> decimal)                                                        
                     spaces
                     modname <- globfileIdent
                     spaces
                     name <- case kind of
                               Notation -> globfileNot
                               _ -> globfileIdent
                     newline                                         
                     return $ GlobFileStatement $ GlobFileRawEntry sbyte (fromMaybe sbyte mebyte) kind "" modname name

globfileResource =  do
                   char 'R'
                   sbyte <- decimal
                   char ':'
                   ebyte <- decimal
                   spaces
                   libname <- globfileIdent
                   spaces
                   modname <- globfileIdent
                   spaces                   
                   name <- globfileIdent <|> globfileNot
                   spaces
                   kind <-     do {string "var"; return Variable}
                           <|> do {string "def"; return Definition}
                           <|> do {string "not"; return Notation}
                           <|> do {string "ind"; return Inductive}
                           <|> do {string "constr"; return Constructor}
                           <|> do {string "thm"; return Theorem}
                           <|> do {string "lib"; return Library}
                           <|> do {string "mod"; return Module}
                           <|> do {string "sec"; return Section}
                   newline
                   return $ GlobFileResource $ GlobFileRawEntry sbyte ebyte kind libname modname name

type PreStatement = StatementA GlobFileRawEntry

data ResourceKind = Resource | StopStatement | IgnorableRes

--  Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof | Library | Module | Section | Inductive

resourceKind :: Kind -> ResourceKind
resourceKind Definition = Resource
resourceKind Theorem = Resource
resourceKind Notation = Resource
resourceKind Tactic = Resource
resourceKind Variable = Resource
resourceKind Constructor = Resource
resourceKind Proof = Resource
resourceKind Library = IgnorableRes -- think of this
resourceKind Module = StopStatement
resourceKind Section = StopStatement
resourceKind Inductive = Resource


loadVernacCode :: String -> Int -> Maybe Int -> IO Code
loadVernacCode vfname pos1 (Just pos2) = do
                                     h <- openBinaryFile vfname ReadMode                                    
                                     hSeek h AbsoluteSeek (fromIntegral pos1)
                                     let sz = pos2-pos1+1
                                     -- buf <- mallocBytes sz :: IO (Ptr Word8)
                                     -- n <- hGetBuf h buf sz
                                     -- bs <- unsafePackCStringFinalizer buf sz (free buf)
                                     bs <- DBS.hGet h sz
                                     return $ CoqText $ TE.decodeUtf8 bs
loadVernacCode vfname pos1 Nothing = return $ CoqText "not implemented"

fromGlobFileRawEntry :: GlobFileRawEntry -> Maybe (Int, PreStatement)
fromGlobFileRawEntry r = case (resourceKind $ ekind r) of
                           Resource ->
                              let fqn = DT.pack $ (elibname r) ++ "." ++ (emodname r) ++ "." ++ (ename r) in
                              Just ((espos r), Statement fqn (ekind r) (CoqText "") [])
                           StopStatement -> Nothing
                           IgnorableRes -> Nothing
                          
collectStatements0 :: [GlobFileEntry] -> String -> Maybe (Int, PreStatement) -> Maybe Int -> [PreStatement] -> IO [PreStatement]
collectStatements0 [] vfname (Just (pos1, cs)) mpos2  accs = do
                                                              cd <- loadVernacCode vfname pos1 mpos2  
                                                              return (cs {code = cd}:accs)
collectStatements0 [] _ Nothing _  accs = return accs
collectStatements0 (s:ss) vfname pcs@(Just (pos1, cs)) mpos2 accs =
            case s of
                GlobFileStatement r-> do                   
                                      cd <- loadVernacCode vfname pos1 (Just $ fromMaybe ((espos r) - 1)  mpos2)
                                      let accs' = (cs {code=cd}:accs)
                                      let newpcs = fromGlobFileRawEntry r
                                      collectStatements0 ss vfname newpcs Nothing accs'                     
                GlobFileResource r ->
                                      case (resourceKind $ ekind r) of
                                        StopStatement -> 
                                                   let newpos2 = Just $ fromMaybe ((espos r) - 1) mpos2 in
                                                   collectStatements0 ss vfname pcs newpos2 accs
                                        Resource -> 
                                                   let newpcs = Just (pos1, cs {uses = r:(uses cs)}) in
                                                   collectStatements0 ss vfname newpcs Nothing accs
                                        IgnorableRes -> collectStatements0 ss vfname pcs Nothing accs
collectStatements0 (s:ss) vfname Nothing _ accs =
            case s of
                GlobFileStatement r ->                                                          
                                      let newpcs = fromGlobFileRawEntry r in
                                      collectStatements0 ss vfname newpcs Nothing accs                     
                GlobFileResource r ->
                                      collectStatements0 ss vfname Nothing Nothing accs



extractStatements :: [String] -> [Statement] -> IO [Statement]
extractStatements [] acc = return acc
extractStatements (f:fs) acc = do
                               -- (_, Just hout, _, _) <- createProcess (proc "coqc" ["-verbose", f]) {std_out = CreatePipe}
                               -- createProcess (proc "coqc" ["-verbose", f++".v"])
                               let vFile = f ++ ".v"
                               let gFile = f ++ ".glob"
                               (ec, s1, _) <- readProcessWithExitCode "coqc" [vFile] []
                               case ec of
                                 ExitFailure _ -> do
                                                   putStrLn ("Error in coqc: " ++ vFile)
                                                   extractStatements fs acc
                                 ExitSuccess ->	 do
                                                   putStrLn ("coqc output:\n" ++ s1)
                                                   globfile  <- readFile gFile                                                   
                                                   case (parse globfileData "" globfile) of
                                                      Left err -> putStrLn "Parse error: " >> print err
                                                      Right (dig, lib, ent)  -> do
                                                                        -- putStrLn $ show ent
                                                                        sts <- collectStatements0 ent vFile Nothing Nothing []
                                                                        let sts' = DL.map (\s -> s{uses = DL.nub $ uses s}) sts 
                                                                        putStrLn $ show sts'
                                                   return acc
