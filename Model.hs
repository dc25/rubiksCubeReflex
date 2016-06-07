module Model where

import Cube
import TwistMode
import Data.Matrix

data Model = Model { cube :: Facet 
                   , orientation :: Matrix Float
                   , twist :: Float
                   , twistMode :: TwistMode
                   }

