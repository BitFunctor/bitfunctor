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
import qualified Data.List as DL 
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified Data.ByteString as DBS (hGet)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.String.Utils as SU (strip, split, replace, join)
import Data.Foldable (foldlM)

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

fromCode :: Code -> Text
fromCode (CoqText t) = t 

data Kind = Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof | Library | Module | Section | Inductive | Axiom
            deriving (Eq, Show, Generic)

instance Binary Text where
  put = put . DT.unpack
  get = get >>= return . DT.pack

instance Binary Code where
  put (CoqText a) = put a
  get = get >>= \a -> return (CoqText a)

-- refactoring needed
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
  put Axiom = putWord8 11
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
      11 -> return Axiom
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
                              , source:: Hash Id
                              , uses :: [a]
                           } deriving (Eq, Show, Generic)

type Statement = StatementA (Hash Id)

instance Binary Statement where
  put s = do
           put (name s)
           put (kind s)
           put (code s)
           put (source s)
           put (uses s)
  get = do n <- get
           k <- get
           c <- get
           sc <- get
           u <- get
           return $ Statement n k c sc u

instance Identifiable Statement where
  id = hash . toStrict . Binary.encode

instance Identifiable Text where
  id = hash . toStrict . Binary.encode

instance Identifiable Kind where
  id = hash . toStrict . Binary.encode


type GlobFileDigest = String

type GlobFileName = String

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

-- TODO: Compare with Coq correct idents
-- .<>[]'_,:=/\\

globfileIdent = do
                 i <-  many1 (letter <|> digit <|> oneOf "._")
                       <|> do {string "<>" ; return ""}
                 return $ trim i

globfileNot = many1 (letter <|> digit <|> oneOf ".<>[]'_,:=/\\+(){}") >>= return . trim

--trim = f . f
--       where f = Prelude.reverse . Prelude.dropWhile isSpace

trim = SU.strip

globfileStatement = do
                     kind <-      do {try (string "def"); return Definition}
                              <|> do {try (string "not"); return Notation}
                              <|> do {try (string "ind"); return Inductive}
                              <|> do {try (string "constr"); return Constructor}
                              <|> do {try (string "prf"); return Proof}
                              <|> do {try (string "mod"); return Module}
                              <|> do {try (string "sec"); return Section}
                              <|> do {try (string "var"); return Variable}
                              <|> do {try (string "ax"); return Axiom}
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
                   kind <-     do {try (string "var");  return Variable}
                           <|> do {try (string "defax");  return Axiom}
                           <|> do {try (string "def");  return Definition}
                           <|> do {try (string "not");  return Notation}
                           <|> do {try (string "ind");  return Inductive}
                           <|> do {try (string "constr");  return Constructor}
                           <|> do {try (string "thm");  return Theorem}
                           <|> do {try (string "lib");  return Library}
                           <|> do {try (string "mod");  return Module}
                           <|> do {try (string "sec");  return Section}
                           <|> do {try (string "prfax");  return Axiom}
                   newline
                   return $ GlobFileResource $ GlobFileRawEntry sbyte ebyte kind libname modname name

type PreStatement = StatementA (Kind, Text)

data ResourceKind = Resource | StopStatement | IgnorableRes
                    deriving (Eq, Show, Generic)

--  Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof | Library | Module | Section | Inductive | Axiom

resourceKind :: Kind -> ResourceKind
resourceKind Definition = Resource
resourceKind Theorem = Resource
resourceKind Notation = IgnorableRes -- Resource cannot print them ATM 
resourceKind Tactic = Resource
resourceKind Variable = Resource
resourceKind Constructor = Resource
resourceKind Proof = Resource
resourceKind Library = IgnorableRes -- think of this
resourceKind Module = StopStatement
resourceKind Section = StopStatement
resourceKind Inductive = Resource
resourceKind Axiom = Resource


-- TODO: deal with multiple declarations without dots like
-- Variale a b: X.
-- Remove vernac comments
loadVernacCode :: String -> Int -> Maybe Int -> IO Code
loadVernacCode vfname pos1 (Just pos2) = do
                                     h <- openBinaryFile vfname ReadMode                                    
                                     hSeek h AbsoluteSeek (fromIntegral pos1)                                     
                                     let seekBackDot n = do
                                                        bs <- DBS.hGet h 1
                                                        -- seeking was at pos1 - (n-1)
                                                        if (TE.decodeUtf8 bs == ".") then return (n-1)
                                                        else do
                                                           -- putStrLn $ show n
                                                           if (pos1 >= n) then do  
                                                              hSeek h AbsoluteSeek (fromIntegral $ pos1 - n)
                                                              seekBackDot (n+1)
                                                           else do
                                                              hSeek h AbsoluteSeek 0
                                                              return n
                                     n <- seekBackDot 1 
                                     let sz = pos2 - pos1 + 1 + n 
                                     -- buf <- mallocBytes sz :: IO (Ptr Word8)
                                     -- n <- hGetBuf h buf sz
                                     -- bs <- unsafePackCStringFinalizer buf sz (free buf)
                                     if (sz > 0) then do 
                                        bs <- DBS.hGet h sz
                                        return $ CoqText $ DT.pack $ DL.dropWhileEnd (\c -> c/='.') $ trim $ DT.unpack $ TE.decodeUtf8 bs
                                     else
                                        return $ CoqText ""
loadVernacCode vfname pos1 Nothing = do
                                       h <- openBinaryFile vfname ReadMode
                                       lastpos <- hFileSize h
                                       loadVernacCode vfname pos1 (Just $ fromIntegral $ lastpos - 1)

fromGlobFileRawEntry :: GlobFileName -> GlobFileRawEntry -> Maybe (Int, PreStatement)
fromGlobFileRawEntry lib r = case (resourceKind $ ekind r) of
                           Resource ->
                              let ln = elibname r in
                              let pref' = if (Prelude.null ln) then lib else ln in
                              let mn = emodname r in
                              let pref = if (Prelude.null mn) then pref' else pref' ++ "." ++ mn in
                              let sn = ename r in
                              if (Prelude.null sn) then Nothing
                                  else   
                                    let fqn = DT.pack $ pref ++ "." ++ sn in
                                    Just ((espos r), Statement fqn (ekind r) (CoqText "") (StatementExtraction.id $ DT.pack "") [])
                           StopStatement -> Nothing
                           IgnorableRes -> Nothing

{--
GlobFileRawEntry {espos:: Int, eepos:: Int, ekind:: Kind, elibname:: String, emodname:: String, ename:: String}
data StatementA a = Statement { name :: Text, kind :: Kind, code :: Code, uses :: [a]} deriving (Eq, Show, Generic)
--}

collectStatements0 :: [GlobFileEntry] -> String -> GlobFileName -> Maybe (Int, PreStatement) -> Maybe Int -> [PreStatement] -> IO [PreStatement]
collectStatements0 [] vfname _ (Just (pos1, cs)) mpos2  accs = do
                                                              cd <- loadVernacCode vfname pos1 mpos2  
                                                              return (cs {code = cd}:accs)
collectStatements0 [] _ _ Nothing _  accs = return accs
collectStatements0 (s:ss) vfname libname pcs@(Just (pos1, cs)) mpos2 accs =
            case s of
                GlobFileStatement r-> 
                                      case (ekind r, kind cs) of
                                        (Constructor, Inductive) ->                                              
                                             let pcons' = fromGlobFileRawEntry libname r in
                                             case pcons' of
                                               Nothing -> fail "Cannot collect constructor"
                                               Just (_, cons') -> 
                                                   let cons = cons' {uses=[(kind cs, name cs)]} in
                                                   collectStatements0 ss vfname libname pcs Nothing (cons:accs)
                                        (Constructor, _) -> fail "Meeting constructor when collecting not-Inductive"
                                        _ -> do
                                               cd <- loadVernacCode vfname pos1 (Just $ fromMaybe ((espos r) - 1)  mpos2)
                                               let accs' = (cs {code=cd}:accs)
                                               let newpcs = fromGlobFileRawEntry libname r
                                               collectStatements0 ss vfname libname newpcs Nothing accs'                     
                GlobFileResource r ->
                                      case (resourceKind $ ekind r) of
                                        StopStatement -> 
                                                   let newpos2 = Just $ fromMaybe ((espos r) - 1) mpos2 in
                                                   collectStatements0 ss vfname libname pcs newpos2 accs
                                        Resource ->
                                                   let r' = fromGlobFileRawEntry libname r in
                                                   case r' of
                                                    Nothing -> collectStatements0 ss vfname libname pcs Nothing accs 
                                                    Just (_, rs) ->
                                                      let newpcs = Just (pos1, cs {uses = (kind rs, name rs):(uses cs)}) in
                                                      collectStatements0 ss vfname libname newpcs Nothing accs
                                        IgnorableRes -> collectStatements0 ss vfname libname pcs Nothing accs
collectStatements0 (s:ss) vfname libname Nothing _ accs =
            case s of
                GlobFileStatement r ->                                                          
                                      let newpcs = fromGlobFileRawEntry libname r in
                                      collectStatements0 ss vfname libname newpcs Nothing accs                     
                GlobFileResource r ->
                                      collectStatements0 ss vfname libname Nothing Nothing accs

collectStatements sts vfname libname = collectStatements0 sts vfname libname Nothing Nothing []


-- think of kind
eqStatement :: PreStatement -> PreStatement -> Bool
eqStatement s1 s2 = (name s1 == name s2) && (kind s1 == kind s2)

{--
GlobFileRawEntry {espos:: Int, eepos:: Int, ekind:: Kind, elibname:: String, emodname:: String, ename:: String}
data StatementA a = Statement { name :: Text, kind :: Kind, code :: Code, uses :: [a]} deriving (Eq, Show, Generic)
--}

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = let (l1,l2) = DL.span p $ DL.reverse l in
              (DL.reverse l2, DL.reverse l1)

-- NB!: if used constructor is declared inside internal module
-- need to look upper 
rereferInductives:: [PreStatement] -> [PreStatement]
rereferInductives sts = let m = Map.fromList $ DL.map (\s -> (name s, s)) sts in
                        let sts' = DL.map (\s -> s {uses = DL.map (\u ->
                                   let look st = let mref = Map.lookup st m in
                                                 case mref of
                                                   Nothing -> let (mod, name) = spanEnd (\c -> c/='.') $ DT.unpack st in
                                                              if (Prelude.null mod) then Nothing
                                                              else 
                                                                 let mod' = DL.dropWhileEnd (\c -> c/='.') $ DL.init mod in
                                                              let st' = DT.pack $ mod' ++ name in
                                                              if (st' == "") then Nothing
                                                                  else look st'
                                                   Just ref -> Just ref
                                   in let mref = look (snd u) in
                                   case mref of
                                     Nothing -> u
                                     Just ref -> (fst u, name ref)
                               ) $ uses s}) sts in
                        DL.map (\s -> s {uses = DL.map (\u -> if (fst u == Constructor) then
                                                           let mcons = Map.lookup (snd u) m in
                                                           case mcons of
                                                             Nothing -> u                                                             
                                                             Just cons -> if (Prelude.null $ uses cons) then u
                                                                          else Prelude.head $ uses cons
                                                              else u) $ uses s}) sts'

-- removes dublicates
-- removes dublicates in "uses"
-- removes self-referencing from "uses"
-- removes Variables from "uses" as they are referenced as Axioms
-- removes Constructors as Statements
adjustStatements :: [PreStatement] -> [PreStatement]
adjustStatements sts = DL.filter (\s -> kind s /= Constructor) $ DL.nubBy eqStatement $
                       DL.map (\s -> s{uses = DL.filter (\u -> u /= (kind s, name s) && fst u /= Variable) $ DL.nub $ uses s} ) $
                       rereferInductives sts

-- data StatementA a = Statement { name :: Text, kind :: Kind, code :: Code, uses :: [a]} deriving (Eq, Show, Generic)
-- ((name, code), filename)
preStatementPair :: PreStatement -> (String, String)
preStatementPair ps = -- ((snd $ spanEnd (\c -> c /= '.') $ DT.unpack $ name ps,
                          (DT.unpack $ fromCode $ code ps,
                          fst $ DL.span (\c -> c /= '.') $ DT.unpack $ name ps)
                             

-- (filecontext, statement, filename)
generateUnresolvedFile:: Kind -> Text -> Map.Map Text PreStatement -> [(String, Text, String)] -> IO (Maybe (String, Text, String))
generateUnresolvedFile k sts thm filem = if (Map.member sts thm) || (resourceKind k /= Resource) then return Nothing
                                         else do
                                       let fqstname = DT.unpack sts
                                       let sname = "SE" ++ (trim $ DL.dropWhile (\c -> c/=' ') $ show $ StatementExtraction.id sts) 
                                       let fwname = "W" ++ sname ++ ".v"
                                       let frname = "R" ++ sname ++ ".v"
                                       let (modname, stname) = spanEnd (\c -> c/='.') fqstname
                                       writeFile fwname ("Require Import " ++ modname ++ "\nPrint " ++ fqstname ++ ".")
                                       (ec, s1, _) <- readProcessWithExitCode "coqc" [fwname] []
                                       case ec of
                                         ExitFailure _ -> do
                                                   putStrLn ("Error in coqc: " ++ fwname)
                                                   return Nothing
                                         ExitSuccess ->	 do
                                                   -- putStrLn ("coqc output:\n" ++ s1)
                                                   let header = "Require Import " ++ modname ++ "\n"                  
                                                   let prebody = trim $ Prelude.head $ SU.split "\n\n" s1
                                                   let body =  (if (k == Definition) || (k == Theorem) then
                                                                "Definition " ++ (SU.join "\n" $
                                                                 (\sl -> ((SU.replace "=" ":=" $ Prelude.head sl):(Prelude.tail sl)))
                                                                 -- incorrect - remove all the type
                                                                 (SU.split "\n" $ trim $ fst $ spanEnd (\c -> c /= '\n') prebody))
                                                               else prebody) ++ "."
                                                   let newst = header ++ body
                                                   let mfile = Map.lookup newst $ Map.fromList $
                                                               DL.map (\(fc, st, fn) -> (fc, (st, fn))) filem
                                                   case mfile of
                                                     Just (_, f) -> return $ Just (newst, sts, f)
                                                     Nothing -> do
                                                                  -- putStrLn $ "Not found." 
                                                                  writeFile frname newst
                                                                  return $ Just (newst, sts, 'R':sname)
                                

--(a -> b -> a) -> a -> [b] -> a
--(b -> a -> m b) -> b -> t a -> m b

generateUnresolvedFiles:: [PreStatement] -> Map.Map Text PreStatement -> IO [(Text, String)]
generateUnresolvedFiles sts thm = do                                     
                                    mfiles <- foldlM (\fm u -> do
                                                               mts <- generateUnresolvedFile (fst u) (snd u) thm fm
                                                               case mts of
                                                                 Nothing -> return fm
                                                                 Just x -> return $ (x:fm)  
                                                         ) [] $ DL.nub $ Prelude.concat $ DL.map uses sts
                                    return $ Map.toList $ Map.fromList $ DL.map (\(fc, st, fn) -> (st, fn)) mfiles
                                        
 
changeStatement :: (Kind, Text) -> Map.Map Text String -> (Kind, Text)
changeStatement (k,t) m = let newst = Map.lookup t m in
                          let (s1,s2) = spanEnd (\c -> c/='.') $ DT.unpack t in
                          case newst of
                           Nothing -> (k,t)
                           Just s -> (k, DT.pack $ s ++ "." ++ s2)

extractStatements :: [String] -> [PreStatement] -> IO [PreStatement]
-- TODO:>
-- finally adjust  - remove all uses of the same Statement (with equal name and source hash)
-- remove such Statement - save only one !
-- remove all not found uses, hoping we are doing well :)
-- convert to Statement
-- remove temporary files (here?)
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
                                                   vText <- readFile vFile                                                  
                                                   case (parse globfileData "" globfile) of
                                                      Left err -> do
                                                                   putStrLn "Parse error: " >> print gFile >> print err
                                                                   extractStatements fs acc
                                                      Right (dig, lib, ent)  -> do
                                                                        -- putStrLn $ show ent
                                                                        -- TODO: read vfile before like globfile - allows to avoid IO
                                                                        sts'' <- collectStatements ent vFile lib
                                                                        let sts' = DL.map (\s -> s{source = StatementExtraction.id $ DT.pack vText}) sts''
                                                                        let sts = adjustStatements sts'
                                                                        let newacc = adjustStatements $ sts' ++ acc
                                                                        let thm = Map.fromList $ DL.map (\s -> (name s, s)) newacc
                                                                        newfiles <- generateUnresolvedFiles sts thm
                                                                        -- let newfiles = DL.nub newfiles'
                                                                        let newnames = Map.fromList newfiles
                                                                        let newacc' = DL.map (\s -> s{uses = DL.map (\u -> changeStatement u newnames) $ uses s}) newacc
                                                                        -- putStrLn $ show newnames
                                                                        putStrLn $ "File " ++ f ++ " has been processed"
                                                                        -- return []
                                                                        extractStatements ((DL.map snd newfiles) ++ fs) newacc'
                                      
