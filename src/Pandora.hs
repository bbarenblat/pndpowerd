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

module Pandora
    ( chargePercent
    , chargeCurrent
    , chargingEnabled
    , setCharging
    ) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))

fuelGauge :: String
fuelGauge = "/sys/class/power_supply/bq27500-0"

acCharger :: String
acCharger = "/sys/class/power_supply/twl4030_ac"

chargePercent :: IO Int
chargePercent = read <$> readFile (fuelGauge </> "capacity")

-- | Determines the charge current in microamperes (µA).
chargeCurrent :: IO Int
chargeCurrent = read <$> readFile (fuelGauge </> "current_now")

chargingEnabled :: IO Bool
chargingEnabled = do
    t <- readFile (acCharger </> "enable")
    return $ case t of
        "0\n" -> False
        "1\n" -> True
        _ -> error "no parse"

setCharging :: Bool -> IO ()
setCharging = writeFile (acCharger </> "enable") . showAsInt

showAsInt :: Bool -> String
showAsInt False = "0"
showAsInt True  = "1"
