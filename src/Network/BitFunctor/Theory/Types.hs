{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Theory.Types where

import qualified Data.Map as Map
import Data.ByteString
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative
import Data.Text


data Code = CoqText Text
            deriving (Eq, Show)

data Kind = Type | Function | Theorem
            deriving (Eq, Show)

data Statement = Statement { name :: Text
                           , code :: Code
                           , kind :: Kind
                           , uses :: [Statement]
                           } deriving (Eq, Show)

type Theory = Map.Map String Statement


instance FromJSON Code where
  parseJSON (Object c) = CoqText <$> c .: "coqText"
  parseJSON invalid    = typeMismatch "Code" invalid

instance ToJSON Code where
 toJSON (CoqText c) = object [ "coqText" .= show c ]


instance FromJSON Kind where
 parseJSON (String "Type")     = return Type
 parseJSON (String "Function") = return Function
 parseJSON (String "Theorem")  = return Theorem
 parseJSON invalid             = typeMismatch "Kind" invalid

instance ToJSON Kind where
 toJSON Type     = toJSON ("Type"     :: String)
 toJSON Function = toJSON ("Function" :: String)
 toJSON Theorem  = toJSON ("Theorem"  :: String)
