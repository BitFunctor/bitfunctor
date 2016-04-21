module Network.BitFunctor.Theory.Coq.Constants  where

import Network.BitFunctor.Theory.Types
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- parser constants

globDigestString = "DIGEST"
globLibChar = 'F'
globResourceChar = 'R'
coqIdentExtraChars = "._'\8322\8321"
coqNotationExtraChars = ".<>[]'_,:=/\\+(){}!?*-|^~&@\8322\8321"
globEmptySubEntryString = "<>"
globLineNumbersDelimiter = ':'
coqModuleDelimiter = '.'
coqStatementDelimiter = '.'
xtrFilePrefix = "SE"
xtrPrintFilePrefix = "WP"
xtrTypeFilePrefix = "WT"
vernacFileSuffix = ".v"
vernacBinaryFileSuffix = ".vo"
globFileSuffix = ".glob"
coqExportString = Text.pack "Require Export "
coqImportString = Text.pack "Import "
coqPrintTermString = Text.pack "Set Printing All.\nUnset Printing Universes.\nSet Printing Depth 1000.\nPrint "
coqExecutable = "coqc"
coqPrintCommentsDelimiter = Text.pack "\n\n"
coqTypeDelimiter = Text.singleton ':'
coqDefinitionString = Text.pack "Definition "
coqAxiomString = Text.pack "Axiom " 
coqPrintEqSign = Text.singleton '='
coqDefSign = Text.pack ":="
generatedFilePrefix = "Bitfunctor"
nullLibString = ("" :: String )
coqSpace = Text.pack " "


(<>) :: Text.Text -> Text.Text -> Text.Text
(<>) t1 t2 = Text.append t1 t2

coqDefineTerm :: Text.Text -> Text.Text -> Text.Text -> Text.Text
coqDefineTerm s t b = if (Text.null b) then
                         coqAxiomString <> s <> coqTypeDelimiter <> t <> (Text.singleton coqStatementDelimiter)
                      else
                         coqDefinitionString <> s <> coqTypeDelimiter <> t <> coqDefSign <> b <> (Text.singleton coqStatementDelimiter)

coqExportLib :: Text.Text -> Text.Text
coqExportLib l | Text.null l = Text.empty
               | otherwise = coqExportString <> l <> (Text.pack ".\n")

coqPrintTerm :: Text.Text -> Text.Text
coqPrintTerm t | Text.null t = Text.empty
               | otherwise =  coqPrintTermString <> t <> (Text.pack ".\n")

coqPrintType :: Text.Text -> Text.Text
coqPrintType t | Text.null t = Text.empty
               | otherwise = coqPrintTermString <> (Text.pack "Implicit ") <> t <> (Text.pack ".\n")

coqImportMod :: Text.Text -> Text.Text
coqImportMod m | Text.null m = Text.empty
               | otherwise =  coqImportString <> m <> (Text.pack ".\n")

-- different parser mappings

globKindStrings = [("defax", (Axiom, Resource)),
                   ("def", (Definition, Resource)),
                   ("not", (Notation, IgnorableRes)),
                   ("ind", (Inductive, Resource)),
                   ("constr", (Constructor, Resource)),
                   ("prfax", (Axiom, Resource)),
                   ("prf", (Proof, Resource)),
                   ("sec", (Section, StopStatement)),
                   ("var", (Variable, Resource)),
                   ("ax", (Axiom, Resource)),
                   ("modtype", (ModType, StopStatement)),
                   ("mod",  (Module, StopStatement)),
                   ("inst", (Instance, Resource)),
                   ("syndef", (SynDef, Resource)),
                   ("class", (Class, Resource)),
                   ("rec", (Record, Resource)),
                   ("proj", (Projection, Resource)),
                   ("meth", (Method, Resource)),
                   ("thm", (Theorem, Resource)),
                   ("lib", (Library, IgnorableRes)),
                   ("scheme", (Scheme, Resource))
                   ]

resourceKind :: CoqKind -> ResourceKind
resourceKind k = let m = Map.fromList $ List.map snd globKindStrings in
                 Map.findWithDefault IgnorableRes k m                           

listAbnormallyPrinted = [Definition, Theorem, Method , Class, Axiom, Proof, Instance, Variable, Scheme]

isAbnormallyPrinted k = List.elem k listAbnormallyPrinted 
