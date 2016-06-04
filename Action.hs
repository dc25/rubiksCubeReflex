module Action (Action(RotateFace,NudgeCube,Animate)) where

import Cube
import Rotation
import Direction

data Action = NudgeCube Direction | RotateFace Rotation Facet | Animate
