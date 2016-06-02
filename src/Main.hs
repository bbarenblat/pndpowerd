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

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Either.Utils (forceEither)
import qualified Config
import qualified Pandora

main :: IO ()
main = do
    config <- forceEither <$> Config.load
    forever $ do
        percent <- Pandora.chargePercent
        current <- Pandora.chargeCurrent
        chargingEnabled <- Pandora.chargingEnabled
        if chargingEnabled
            && Config.stopCapacity config <= percent
            && 0 < current
            && current < Config.stopCurrent config
            then Pandora.setCharging False
            else when (not chargingEnabled
                       && percent <= Config.stopCapacity config) $
                Pandora.setCharging True
        threadDelay (secondsToMicroseconds 60)

secondsToMicroseconds :: Int -> Int
secondsToMicroseconds = (* 1000000)
