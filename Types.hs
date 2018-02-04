module Types
   ( Map
   , Data
   , nullData
   , module Export
   )
where

import Data.Monoid as Export
import Data.Map (Map)

--------------------------------------------------------------------------------

type Data = String -- to be replaced by bytestring (or something)

nullData = "" :: Data
