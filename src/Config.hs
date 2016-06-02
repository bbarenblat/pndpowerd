-- Copyright 2016 Benjamin Barenblat
--
-- Licensed under the Apache License, Version 2.0 (the “License”); you may not
-- use this file except in compliance with the License.  You may obtain a copy
-- of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an “AS IS” BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
-- License for the specific language governing permissions and limitations under
-- the License.

-- | Application configuration.

module Config
    ( Config(..)
    , load
    ) where

import Control.Applicative ((<$>))
import Data.Bifunctor (bimap)
import qualified Data.ConfigFile as ConfigFile

data Config = Config
    { stopCapacity :: Int
        -- ^ percentage charge required to enter the “fully charged” state
    , stopCurrent :: Int
        -- ^ maximum charge current (in microamperes, µA) to remain in the
        -- “fully charged” state
    , startCapacity :: Int
        -- ^ percentage at or below which charging will begin
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { stopCapacity = 99
    , stopCurrent = 80000
    , startCapacity = 95
    }

configFilePath :: FilePath
configFilePath = "/etc/pndpowerd.conf"

load :: IO (Either String Config)
load =
    bimap show parse <$> ConfigFile.readfile ConfigFile.emptyCP configFilePath

parse :: ConfigFile.ConfigParser -> Config
parse serialized =
    let get field name = either (const $ field defaultConfig) id $
            ConfigFile.get serialized "DEFAULT" name
    in Config { stopCapacity = get stopCapacity "stopCapacity"
              , stopCurrent = get stopCurrent "stopCurrent"
              , startCapacity = get startCapacity "startCapacity"
              }
