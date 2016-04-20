module Network.BitFunctor.Theory.Coq.GlobFileParser  where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec.Number (decimal, int, nat)
import qualified Data.String.Utils as SU
import Data.Maybe (fromMaybe)

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.Constants as Constants

-- the internal Coq hash
type GlobFileDigest = String
-- the name of library  - the same as the file name
type GlobFileName = String

data GlobFilePosition = GlobFilePosition {espos:: Int, eepos:: Int} deriving (Eq, Show)
-- the general entry inside .glob file
data GlobFileRawEntry = GlobFileRawEntry {epos:: GlobFilePosition, 
                                          ekind:: CoqKind, -- the kind of the entry
                                          elibname:: String, -- library name
                                          emodname:: String, -- module name
                                          ename:: String} -- name of statement/entry
                        deriving (Eq, Show)

-- the entry could be either statement or resource to some statement
data GlobFileEntry = GlobFileResource GlobFileRawEntry | GlobFileStatement GlobFileRawEntry
                     deriving (Eq, Show)
-- data for parser
type GlobFileData = (GlobFileDigest, GlobFileName, [GlobFileEntry])

-- parser for the whole file
globfileData :: Parser GlobFileData
globfileData = do
                dig <- globfileDigest
                newline
                name <- globfileName
                newline
                sts <- many (globfileStatement <|> globfileResource)
                return (dig, name, sts)

-- 
globfileDigest = string Constants.globDigestString >> spaces >> globfileIdent 
globfileName = char Constants.globLibChar >> globfileIdent

-- TODO: Compare with Coq correct idents
globfileIdent = do
                 i <-  many1 (letter <|> digit <|> oneOf Constants.coqIdentExtraChars)
                       <|> do {string Constants.globEmptySubEntryString ; return ""}
                 return $ SU.strip i

globfileNot = many1 (letter <|> digit <|> oneOf Constants.coqNotationExtraChars) >>= return . SU.strip

parseFromList :: (a -> b) -> [(String, a)] -> Parser b
parseFromList _ [] = fail "Cannot parse from empty patterns list or patterns not exhaustive"
parseFromList f (p:ps) = do {try (string $ fst p); return $ f $ snd p } <|>
                         parseFromList f ps

globfileStatement = do
                     kind <- parseFromList fst Constants.globKindStrings
                     spaces
                     sbyte <- decimal
                     mebyte <- optionMaybe (char Constants.globLineNumbersDelimiter >> decimal)
                     spaces
                     modname <- globfileIdent
                     spaces
                     -- notations are parsed with less restrictions to the char list
                     name <- case kind of
                               Notation -> globfileNot
                               _ -> globfileIdent
                     newline                                         
                     return $ GlobFileStatement $ GlobFileRawEntry (GlobFilePosition sbyte (fromMaybe sbyte mebyte)) kind "" modname name

globfileResource =  do
                     char Constants.globResourceChar
                     sbyte <- decimal
                     char Constants.globLineNumbersDelimiter
                     ebyte <- decimal
                     spaces
                     libname <- globfileIdent
                     spaces
                     modname <- globfileIdent
                     spaces                   
                     name <- globfileIdent <|> globfileNot
                     spaces
                     kind <- parseFromList fst Constants.globKindStrings
                     newline
                     return $ GlobFileResource $ GlobFileRawEntry (GlobFilePosition sbyte ebyte) kind libname modname name
