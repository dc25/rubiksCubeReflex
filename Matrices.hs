module Matrices ( identityMatrix
                , xyRotationMatrix
                , yzRotationMatrix
                , zxRotationMatrix
                , scaleMatrix
                , translationMatrix
                , perspectiveMatrix
                , perspectivePrepMatrix
                ) where

import Prelude(Float,cos,sin)
import Data.Matrix (Matrix, fromLists)

identityMatrix :: Matrix Float
identityMatrix = 
    fromLists [[ 1,  0,  0,  0 ]
              ,[ 0,  1,  0,  0 ]
              ,[ 0,  0,  1,  0 ]
              ,[ 0,  0,  0,  1 ]
              ]

xyRotationMatrix :: Float -> Matrix Float
xyRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  s,  0,  0 ]
                 ,[-s,  c,  0,  0 ]
                 ,[ 0,  0,  1,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

yzRotationMatrix :: Float -> Matrix Float
yzRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ 1,  0,  0,  0 ]
                 ,[ 0,  c,  s,  0 ]
                 ,[ 0, -s,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

zxRotationMatrix :: Float -> Matrix Float
zxRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  0,  s,  0 ]
                 ,[ 0,  1,  0,  0 ]
                 ,[-s,  0,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

translationMatrix :: (Float,Float,Float) -> Matrix Float
translationMatrix (x,y,z) =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  0 ]
               ,[ x,  y,  z,  1 ]
               ]

scaleMatrix :: Float -> Matrix Float
scaleMatrix s =
    fromLists  [[ s,  0,  0,  0 ]
               ,[ 0,  s,  0,  0 ]
               ,[ 0,  0,  s,  0 ]
               ,[ 0,  0,  0,  1 ]
               ]

-- translate model to (0,0,1) for perspective viewing
perspectivePrepMatrix :: Matrix Float
perspectivePrepMatrix = translationMatrix (0,0,1)

-- perspective transformation 
perspectiveMatrix :: Matrix Float
perspectiveMatrix = 
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  1 ]
               ,[ 0,  0,  0,  0 ] ]

