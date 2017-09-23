module Network.BitFunctor2.Platform.Application where

import Data.UUID
import Data.Text (Text)


type ApplicationId = UUID

data ApplicationInfo = ApplicationInfo
  { applicationInfoName        :: Text
  , applicationInfoDescription :: Text
  , applicationInfoIds         :: [ApplicationId]
  } deriving (Eq, Show)


class Application a where
  getInfo :: a -> ApplicationInfo


-- data SupportedApplications = SupportedApplications
--   { supportedApplications :: ApplicationId -> ApplicationHandle
--    }

data PlatformApplication = PlatformApplication
  { paInfo :: ApplicationInfo

  }
