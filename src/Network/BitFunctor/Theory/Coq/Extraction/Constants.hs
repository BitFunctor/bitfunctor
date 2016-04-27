{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.BitFunctor.Theory.Coq.Extraction.Constants  where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Monoid ((<>))

import Network.BitFunctor.Common
import Network.BitFunctor.Theory.Coq.Types


globDigestString = "DIGEST"
globLibChar = 'F'
globResourceChar = 'R'
coqIdentExtraChars = "._'\8322\8321"
coqNotationExtraChars = ".<>[]'_,:=/\\+(){}!?*-|^~&@\8322\8321"
globEmptySubEntryString = "<>"
globLineNumbersDelimiter =  ':'
coqModuleDelimiter = '.'
coqStatementDelimiter =  '.'
xtrFilePrefix = "SE"
xtrPrintFilePrefix = "WP"
xtrTypeFilePrefix = "WT"
vernacFileSuffix = ".v"
vernacBinaryFileSuffix = ".vo"
globFileSuffix = ".glob"
coqExportString = "Require Export "
coqImportString =  "Import "
coqPrintTermString = "Set Printing All.\nUnset Printing Universes.\nSet Printing Depth 1000.\nPrint "
coqExecutable = "coqc"
coqPrintCommentsDelimiter :: Text.Text =  "\n\n"
coqTypeDelimiter = Text.singleton ':'
coqDefinitionString = "Definition "
coqAxiomString = "Axiom " 
coqPrintEqSign = Text.singleton  '='
coqDefSign =  ":="
generatedFilePrefix = "Bitfunctor"
nullLibString = ("" :: String )
coqSpace :: Text.Text  = " "


coqDefineTerm :: Text.Text -> Text.Text -> Text.Text -> Text.Text
coqDefineTerm s t b = if (Text.null b) then
                         coqAxiomString <> s <> coqTypeDelimiter <> t <> (Text.singleton coqStatementDelimiter)
                      else
                         coqDefinitionString <> s <> coqTypeDelimiter <> t <> coqDefSign <> b <>  (Text.singleton coqStatementDelimiter)

coqExportLib :: Text.Text -> Text.Text
coqExportLib l | Text.null l = Text.empty
               | otherwise = coqExportString <> l <> ".\n"

coqPrintTerm :: Text.Text -> Text.Text
coqPrintTerm t | Text.null t = Text.empty
               | otherwise =  coqPrintTermString <> t <> ".\n"

coqPrintType :: Text.Text -> Text.Text
coqPrintType t | Text.null t = Text.empty
               | otherwise = coqPrintTermString <> "Implicit " <> t <> ".\n"

coqImportMod :: Text.Text -> Text.Text
coqImportMod m | Text.null m = Text.empty
               | otherwise =  coqImportString <> m <> ".\n"

fullPrintTerm l1 l2 m t = let l2' = if l1 == l2 then "" else l2 in
   (coqExportLib l1) <> (coqExportLib l2') <> (coqImportMod m) <> (coqPrintTerm t)

fullPrintType l1 l2 m t = let l2' = if l1 == l2 then "" else l2 in
   (coqExportLib l1) <> (coqExportLib l2') <> (coqImportMod m) <> (coqPrintType t)

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
