import Reflex.Dom
import Data.Map
import Data.List
import Data.Monoid ((<>))

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show,Eq,Ord)

data DNode a = DNode { north :: DNode a
                     , west  :: DNode a
                     , south :: DNode a
                     , east  :: DNode a
                     , val   :: a
                     , index :: Int
                     }

signature :: DNode a -> (a, Int)
signature dn = (val dn, index dn)

instance Eq a => Eq (DNode a) where 
    d0 == d1 = (signature d0) == (signature d1) 

instance Ord a => Ord (DNode a) where 
    compare d0 d1 = compare (signature d0) (signature d1) 

type Facet = DNode Color
type Edge = (Facet,Facet,Facet)

mkDomino :: Facet -> Facet -> Facet -> Facet -> Facet -> Facet -> Color -> Int -> (Facet,Facet)
mkDomino right upperRight upperLeft left lowerLeft lowerRight color index =
    let rightDomino = DNode upperRight leftDomino lowerRight right       color  index
        leftDomino =  DNode upperLeft  left       lowerLeft  rightDomino color (index+1)
    in (leftDomino, rightDomino)

mkFace :: Edge -> Edge -> Edge -> Edge -> Color -> (Edge,Edge,Edge,Edge)
mkFace ~(nRight, nCenter, nLeft) 
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
       , (enCorner, eSide, seCorner)
       )

mkCube = 
    let (nPurple, wPurple, sPurple,  ePurple) = mkFace nGreen    nBlue     nYellow   nRed     Purple

        (nYellow, wYellow, sYellow,  eYellow) = mkFace sPurple   eBlue     nOrange   wRed     Yellow
        (nBlue,   wBlue,   sBlue,    eBlue)   = mkFace wPurple   eGreen    wOrange   wYellow  Blue
        (nGreen,  wGreen,  sGreen,   eGreen)  = mkFace nPurple   eRed      sOrange   wBlue    Green
        (nRed,    wRed,    sRed,     eRed)    = mkFace ePurple   eYellow   eOrange   wGreen   Red

        (nOrange, wOrange, sOrange, eOrange)  = mkFace sYellow   sBlue     sGreen    sRed     Orange
        (_,cube,_) = nPurple
    in south cube

copyNode :: Facet -> Facet
copyNode f = 
    DNode (copyNode $ north f )
          (copyNode $ west f )
          (copyNode $ south f )
          (copyNode $ east f )
          (val f)
          (index f)

type RotationMap = Map (Facet, Facet) Facet 

copyWithRotation :: RotationMap -> Facet -> Facet
copyWithRotation rotationMap f = 
    DNode (copyWithRotation rotationMap $ checkForRotation rotationMap f $ north f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ west f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ south f )
          (copyWithRotation rotationMap $ checkForRotation rotationMap f $ east f )
          (val f)
          (index f)
    where 
        checkForRotation rotationMap startFacet preRotationFacet =
            case Data.Map.lookup (startFacet, preRotationFacet) rotationMap
            of Nothing -> preRotationFacet
               Just postRotationFacet -> postRotationFacet

getRotationMap :: Facet -> RotationMap
getRotationMap f =
    let 
        ff :: (Facet, Facet, RotationMap) -> (Facet -> Facet, Facet -> Facet) -> (Facet, Facet, RotationMap)
        ff (pre, post, oldRotMap) (splitDown, advanceDirection) =
            let rm' =  Data.Map.insert (pre, splitDown pre) (splitDown post) oldRotMap
                newRotMap =  Data.Map.insert (splitDown post, post) pre rm'
            in (advanceDirection pre, advanceDirection post, newRotMap)

        crawlDirections = concat $ replicate 4 [(east, south), (south, west), (south, west)]
        (_,_,rm) = Data.List.foldl ff ((east.north.north) f, (west.west.south.east.north.north) f, Data.Map.empty) crawlDirections

    in rm

rotateFace :: Facet -> Facet
rotateFace f = copyWithRotation (getRotationMap f) f

width = 200
height = 200

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"


showFacet :: MonadWidget t m => Facet -> Int -> Int -> m (Event t ())
showFacet facet row col = do
    (el, _) <- elDynAttrNS' svgNamespace "rect" 
                   (constDyn $  "x" =: show col
                             <> "y" =: show row
                             <> "width" =: "1" 
                             <> "height" =: "1" 
                             <> "fill" =: (show $ val facet))
               $ return ()
    return $ domEvent Click el 

showFace upperLeft = do
        (_, ev) <- elDynAttrNS' svgNamespace "svg" 
                        (constDyn $  "viewBox" =: ("0 0 3 3 ")
                                  <> "width" =: show width
                                  <> "height" =: show height)
                        $ do 
                             showFacet upperLeft 0 0 
                             showFacet (east upperLeft) 0 1 

                             let upperRight = east $ east upperLeft
                             showFacet upperRight 0 2 
                             showFacet (east upperRight) 1 2 

                             let lowerRight = east $ east upperRight
                             showFacet lowerRight 2 2 
                             showFacet (east lowerRight) 2 1 

                             let lowerLeft = east $ east lowerRight
                             showFacet lowerLeft 2 0 
                             showFacet (east lowerLeft) 1 0 

                             let center = south $ east upperLeft
                             showFacet center 1 1 


        return ()

floatLeft = "style" =: "float:left" 
clearLeft = "style" =: "clear:left" 

showCube cube = do
        let purpleFace = (west.north) cube
        el "div" $ showFace  purpleFace

        let yellowFace = west $ west $ south purpleFace
        elAttr "div" floatLeft $ showFace yellowFace

        let redFace = north $  east $ east yellowFace
        elAttr "div" floatLeft $ showFace redFace

        let greenFace = north $  east $ east redFace
        elAttr "div" floatLeft $ showFace greenFace

        let blueFace = north $  east $ east greenFace
        elAttr "div" floatLeft $ showFace blueFace 

        let orangeFace = west $  west $ south yellowFace
        elAttr "div" clearLeft $ showFace orangeFace

        return ()


view cube = do 
            let purpleFace = cube
                pRot = rotateFace $ rotateFace purpleFace

                orangeFace = south $  north $ south $ south $  north $ south pRot
                oRot = rotateFace $ rotateFace orangeFace

                yellowFace = south $  north $ north oRot
                yRot = rotateFace $ rotateFace yellowFace

                greenFace = west $  west $ south $ west $  west $ south yRot
                gRot = rotateFace $ rotateFace yellowFace
            showCube yRot

            return ()


main = 
          let  cube = mkCube
          in mainWidget $ do 
               view cube

