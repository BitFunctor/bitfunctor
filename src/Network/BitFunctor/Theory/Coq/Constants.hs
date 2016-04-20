module Network.BitFunctor.Theory.Coq.Constants  where

import Network.BitFunctor.Theory.Types
import qualified Data.Map as Map
import qualified Data.List as List

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
coqExportString = "Require Export "
coqImportString = "Import "
coqPrintTermString = "Set Printing All.\nUnset Printing Universes.\nSet Printing Depth 1000.\nPrint "
coqExecutable = "coqc"
coqPrintCommentsDelimiter = "\n\n"
coqTypeDelimiter = ":"
coqDefinitionString = "Definition "
coqAxiomString = "Axiom "
coqPrintEqSign = "="
coqDefSign = ":="
generatedFilePrefix = "Bitfunctor"
nullLibString = ("" :: String )
coqSpace = " "

coqDefineTerm s t b = if (Prelude.null b) then
                         coqAxiomString ++ s ++ coqTypeDelimiter ++ t ++ [coqStatementDelimiter]
                      else
                         coqDefinitionString ++ s ++ coqTypeDelimiter ++ t ++ coqDefSign ++ b ++ [coqStatementDelimiter]

coqExportLib "" = ""
coqExportLib l = coqExportString ++ l ++ ".\n"

coqPrintTerm "" = ""
coqPrintTerm t = coqPrintTermString ++ t ++ ".\n"

coqPrintType "" = ""
coqPrintType t = coqPrintTermString ++ "Implicit " ++ t ++ ".\n"

coqImportMod "" = ""
coqImportMod m = coqImportString ++ m ++ ".\n"

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
                   ("scheme", (Scheme, IgnorableRes))
                   ]

resourceKind :: CoqKind -> ResourceKind
resourceKind k = let m = Map.fromList $ List.map snd globKindStrings in
                 Map.findWithDefault IgnorableRes k m                           

listAbnormallyPrinted = [Definition, Theorem, Method , Class, Axiom, Proof, Instance, Variable]

isAbnormallyPrinted k = List.elem k listAbnormallyPrinted 
