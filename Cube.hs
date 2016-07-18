module Cube ( DNode(DNode)
            , Facet

            , mkCube

            , north
            , west
            , south
            , east
            , color
            , index

            ) where

import Data.Map (Map, lookup, insert, empty)
import Data.List (foldl)
import Data.Maybe (fromMaybe)

import Rotation
import Color

data DNode a = DNode { north :: DNode a
                     , west  :: DNode a
                     , south :: DNode a
                     , east  :: DNode a
                     , color :: a
                     , index :: Int
                     }

type Signature a = (a, Int)

signature :: DNode a -> Signature a
signature dn = (color dn, index dn)

instance Eq a => Eq (DNode a) where 
    d0 == d1 = signature d0 == signature d1 

instance Ord a => Ord (DNode a) where 
    compare d0 d1 = compare (signature d0) (signature d1) 

type Facet = DNode Color
type Edge = (Facet,Facet,Facet)
---                       upperLeft   upperRight
---                    ___________________________
---                    |     N      |     N      |
---                    |            |            |
---               left |W  leftDo  E|W  rightDo E| right
---                    |            |            |
---                    |            |            |
---                    |_____S______|_____S______|
---
---                       lowerLeft   lowerRight

mkDomino :: Facet -> Facet -> Facet -> Facet -> Facet -> Facet -> Color -> Int -> (Facet,Facet)
mkDomino right upperRight upperLeft left lowerLeft lowerRight color index =
    let leftDo =  DNode upperLeft  left   lowerLeft  rightDo color index
        rightDo = DNode upperRight leftDo lowerRight right   color (index+1)
    in (leftDo, rightDo)

---                        nLeft        nCenter      nRight
---                    ____________ ___________________________
---                    |     N      |     N      |     W      |
---                    |            |            |            |
---           wRight   |W nwCorner E|W  nSide   E|S enCorner N|  eLeft
---                    |            |            |            |
---                    |            |            |            |
---                    |_____S______|_____S______|_____E______|
---                    |     E      |     N      |     W      |
---                    |            |            |            |
---           wCenter  |N  wSide   S|W  center  E|S  eSide   N|  eCenter
---                    |            |            |            |
---                    |            |            |            |
---                    |_____W______|_____S______|_____E______|
---                    |     E      |     S      |     S      |
---                    |            |            |            |
---           wLeft    |N wsCorner S|E  sSide   W|E seCorner W|  eRight
---                    |            |            |            |
---                    |            |            |            |
---                    |_____W______|_____N______|_____N______|
---
---                        sRight       sCenter      sLeft 

mkFace :: Edge -> Edge -> Edge -> Edge -> Color -> (Edge,Edge,Edge,Edge)
mkFace ~(nRight, nCenter, nLeft) -- use of '~' specifies lazy evaluation of arguments.
       ~(wRight, wCenter, wLeft) 
       ~(sRight, sCenter, sLeft) 
       ~(eRight, eCenter, eLeft) 
       color =
    let center = DNode nSide wSide sSide eSide color 0
        (nwCorner, nSide) = mkDomino enCorner nCenter nLeft wRight wSide center color 1
        (wsCorner, wSide) = mkDomino nwCorner wCenter wLeft sRight sSide center color 3
        (seCorner, sSide) = mkDomino wsCorner sCenter sLeft eRight eSide center color 5
        (enCorner, eSide) = mkDomino seCorner eCenter eLeft nRight nSide center color 7
    in ( (nwCorner, nSide, enCorner)
       , (wsCorner, wSide, nwCorner)
       , (seCorner, sSide, wsCorner)
       , (enCorner, eSide, seCorner))

--- ______________
--- |     N      |
--- |            |
--- |W  purple  E|
--- |            |
--- |            |
--- |_____S_____ |_______________________________________
--- |     N      |     W      |     S      |     E      |
--- |            |            |            |            |
--- |W  yellow  E|S   red    N|E   green  W|N   blue   S|
--- |            |            |            |            |
--- |            |            |            |            |
--- |_____S______|_____E______|_____N______|_____W______|
--- |     N      |
--- |            |
--- |W  orange  E|
--- |            |
--- |            |
--- |_____S______|

mkCube = 
    let (nPurple, wPurple, sPurple,  ePurple) = mkFace sGreen    eBlue     nYellow   wRed     Purple
        (nGreen,  wGreen,  sGreen,   eGreen)  = mkFace sOrange   nBlue     nPurple   nRed     Green
        (nOrange, wOrange, sOrange,  eOrange) = mkFace sYellow   wBlue     nGreen    eRed     Orange
        (nYellow, wYellow, sYellow,  eYellow) = mkFace sPurple   sBlue     nOrange   sRed     Yellow
        (nBlue,   wBlue,   sBlue,    eBlue)   = mkFace wGreen    wOrange   wYellow   wPurple  Blue
        (nRed,    wRed,    sRed,     eRed)    = mkFace eGreen    ePurple   eYellow   eOrange  Red
        (_,cube,_) = nPurple
    in south cube
