{-# LANGUAGE OverloadedStrings #-}

import System.Console.ArgParser
import Control.Applicative
import Data.Text
import Data.Monoid ((<>))

data MyTest = MyTest [Text] [Text]
  deriving (Show) -- we will print the values

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  `parsedBy` optFlagArgs [] "e" [] (\b a -> b ++ [a])  `Descr` "description for the first argument"
  `andBy` optFlagArgs [] "f" [] (\b a -> b ++ [a]) `Descr` "description for the second argument"

myTestInterface :: IO (CmdLnInterface MyTest)
myTestInterface =
  (`setAppDescr` "top description")
  <$> (`setAppEpilog` "bottom description")
  <$> mkApp myTestParser

main = do
  interface <- myTestInterface
  runApp interface print
