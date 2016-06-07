module Update(update) where

import Prelude(Float,pi,($),(+),(-),(*),(/),(==),(<),(>),(.))
import Data.Matrix (Matrix,multStd2)

import Matrices
import Action
import Direction
import Rotation
import TwistMode
import Model
import Cube
import View
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
            let (topFacet, twistMode) = 
                    if facetFacesCamera model facet
                    then (facet, TopTwist)
                    else ((south.east. north.east.east. north.west.north) facet, BottomTwist) 
                twist 
                    | ((rotation,twistMode) == (CW,  TopTwist)) = 90
                    | ((rotation,twistMode) == (CCW, TopTwist)) = (-90)
                    | ((rotation,twistMode) == (CW,  BottomTwist)) = (-90)
                    | ((rotation,twistMode) == (CCW,  BottomTwist)) = 90
            in model { cube = rotateFace rotation facet topFacet
                     , twist = twist
                     , twistMode = twistMode }
        NudgeCube direction -> 
            let step = pi/20
            in case direction of
                   Left ->  rotateModel (zxRotationMatrix (-step) ) model
                   Right -> rotateModel (zxRotationMatrix   step  ) model
                   Up ->    rotateModel (yzRotationMatrix (-step) ) model
                   Down ->  rotateModel (yzRotationMatrix   step  ) model
 
