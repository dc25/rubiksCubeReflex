{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom (mainWidget,tickLossy,foldDyn,leftmost)
import Data.Matrix (multStd2)
import Control.Monad(fmap,return)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

import Matrices
import View
import Action
import Model
import TwistMode
import Cube
import Update

main = mainWidget $ do 
    let initialOrientation =             identityMatrix 
                              `multStd2` zxRotationMatrix (pi/8) 
                              `multStd2` yzRotationMatrix (pi/8) 
        dt = 0.4

    now <- liftIO getCurrentTime
    tick <- tickLossy dt now
    let advanceAction = fmap (const Animate) tick
    rec
        selectAction <- view model
        model <- foldDyn update (Model mkCube initialOrientation 0 NoTwist) $ leftmost [selectAction, advanceAction]
    return ()
