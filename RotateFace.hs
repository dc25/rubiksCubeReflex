module RotateFace ( rotateFace) where

import Prelude(Eq, Ord, (==), ($), (.), (+), compare, concat, replicate, Show, Enum, Int)
import Data.Map (Map, lookup, insert, empty)
import Data.List (foldl)
import Data.Maybe (fromMaybe)

import Cube
import Rotation

type RotationMap = Map (Facet, Facet) Facet 

copyWithRotation :: RotationMap -> Facet -> Facet
copyWithRotation rotationMap f = 
    DNode (copyWithRotation rotationMap $ checkForRotation rotationMap f $ north f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ west f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ south f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ east f )
          (color f)
          (index f)
    where 
        checkForRotation rotationMap startFacet preRotationFacet =
            fromMaybe preRotationFacet $ lookup (startFacet, preRotationFacet) rotationMap

getRotationMap :: (Facet -> Facet) -> Facet -> RotationMap
getRotationMap advanceToPost f =
    let preStart = north.north $ f -- from center to edge of adjacent face.
        postStart = advanceToPost preStart  -- clockwise or counterclockwise
        fullCrawl = concat $ replicate 4 [(south, west), (south, west), (east, south)]

        ff :: (Facet, Facet, RotationMap) -> (Facet -> Facet, Facet -> Facet) -> (Facet, Facet, RotationMap)
        ff (pre, post, oldRotMap) (splitDown, advanceDirection) =
            let rm' =  insert (pre, splitDown pre) (splitDown post) oldRotMap
                newRotMap =  insert (splitDown post, post) pre rm'
            in (advanceDirection pre, advanceDirection post, newRotMap)

        (_,_,rotationMap) = foldl ff (preStart, postStart, empty) fullCrawl
    in rotationMap

rotateFace :: Rotation -> Facet -> Facet -> Facet
rotateFace rotation rotatedFace newTop = 
        let advancer = if rotation == CW then south.west.west else east.north.east
        in copyWithRotation (getRotationMap advancer rotatedFace) newTop

