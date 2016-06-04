module Update(update) where

import Prelude(Float,pi,($),(+),(-),(*),(/),(==),(<),(>))
import Data.Matrix (Matrix,multStd2)

import Matrices
import Action
import Direction
import Rotation
import Model
import RotateFace

applyRotation :: Matrix Float -> Matrix Float -> Matrix Float
applyRotation rotationMatrix  prevOrientation = 
    prevOrientation `multStd2` rotationMatrix

rotateModel rotationMatrix model = 
    model { orientation = applyRotation rotationMatrix $ orientation model }

untwist model = 
    let prev_twist = twist model
        new_twist
            | prev_twist == 0 = 0
            | prev_twist > 0 = prev_twist - 30
            | prev_twist < 0 = prev_twist + 30
    in model { twist = new_twist }

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
    case action of
        Animate ->
            untwist model
        RotateFace rotation facet -> 
            model { cube = rotateFace rotation facet, twist = if rotation == CW then 90 else (-90) }
        NudgeCube direction -> 
            let step = pi/20
            in case direction of
                   Left ->  rotateModel (zxRotationMatrix (-step) ) model
                   Right -> rotateModel (zxRotationMatrix   step  ) model
                   Up ->    rotateModel (yzRotationMatrix (-step) ) model
                   Down ->  rotateModel (yzRotationMatrix   step  ) model
 
