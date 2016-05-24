{-# LANGUAGE RecursiveDo #-}
import Prelude(Eq,Ord(compare),Show,Enum,Num,Float,Int,String,Maybe(Just),fst,const,show,fromIntegral,replicate,concat,(.),($),(+),(-),(*),(==),(<$>))
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, elem)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Matrix ()
import Data.Monoid ((<>))
import Control.Monad.Reader

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show,Eq,Ord,Enum)

data Vector3 a = Vector3 { x :: a
                         , y :: a 
                         , z :: a
                         }

cross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 x0 y0 z0) (Vector3 x1 y1 z1)= Vector3 (y0*z1 - y1*z0) (z0*x1 - z1*x0) (x0*y1 - x1*y0)

data DNode a = DNode { north :: DNode a
                     , west  :: DNode a
                     , south :: DNode a
                     , east  :: DNode a
                     , val   :: a
                     , index :: Int
                     }

type Signature a = (a, Int)
type FacetSig = Signature Color


signature :: DNode a -> Signature a
signature dn = (val dn, index dn)

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
--- ______________
--- |     N      |
--- |            |
--- |W  purple  E|
--- |            |
--- |            |
--- |_____S_____ |_______________________________________
--- |     N      |     N      |     N      |     N      |
--- |            |            |            |            |
--- |W  yellow  E|W   blue   E|W   green  E|W   red    E|
--- |            |            |            |            |
--- |            |            |            |            |
--- |_____S______|_____S______|_____S______|_____S______|
--- |     N      |
--- |            |
--- |W  orange  E|
--- |            |
--- |            |
--- |_____S______|

mkCube = 
    let (nPurple, wPurple, sPurple,  ePurple) = mkFace nGreen    nBlue     nYellow   nRed     Purple

        (nYellow, wYellow, sYellow,  eYellow) = mkFace sPurple   eBlue     nOrange   wRed     Yellow
        (nBlue,   wBlue,   sBlue,    eBlue)   = mkFace wPurple   eGreen    wOrange   wYellow  Blue
        (nGreen,  wGreen,  sGreen,   eGreen)  = mkFace nPurple   eRed      sOrange   wBlue    Green
        (nRed,    wRed,    sRed,     eRed)    = mkFace ePurple   eYellow   eOrange   wGreen   Red

        (nOrange, wOrange, sOrange, eOrange)  = mkFace sYellow   sBlue     sGreen    sRed     Orange
        (_,cube,_) = nPurple
    in south cube

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
            fromMaybe preRotationFacet
                (lookup (startFacet, preRotationFacet) rotationMap)

getRotationMap :: (Facet -> Facet) -> Facet -> RotationMap
getRotationMap advanceToPost f =
    let 
        ff :: (Facet, Facet, RotationMap) -> (Facet -> Facet, Facet -> Facet) -> (Facet, Facet, RotationMap)
        ff (pre, post, oldRotMap) (splitDown, advanceDirection) =
            let rm' =  insert (pre, splitDown pre) (splitDown post) oldRotMap
                newRotMap =  insert (splitDown post, post) pre rm'
            in (advanceDirection pre, advanceDirection post, newRotMap)

        faceCrawl = [(east, south), (south, west), (south, west)]
        fullCrawl = concat $ replicate 4 faceCrawl
        preStart = east.north.north $ f
        postStart = advanceToPost preStart  -- clockwise or counterclockwise
        (_,_,rotationMap) = foldl ff (preStart, postStart, empty) fullCrawl

    in rotationMap

rotateFace :: Rotation -> Facet -> Facet
rotateFace direction f = 
        let advancer = 
                if direction == CCW 
                then east.east.north 
                else west.west.south
        in copyWithRotation (getRotationMap advancer f) f

rotateFaceCCW :: Facet -> Facet
rotateFaceCCW f = copyWithRotation (getRotationMap (east.east.north) f) f

width = 175
height = 175

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t String -> m ()
showFacetSquare row col margin dColor = do
    attrs <- mapDyn (\color ->    "x" =: show ((fromIntegral col :: Float) + margin)
                               <> "y" =: show ((fromIntegral row :: Float) + margin)
                               <> "width" =: show (1.0 - 2.0 * margin)
                               <> "height" =: show (1.0 - 2.0 * margin)
                               <> "fill" =: color) dColor

    elDynAttrNS' svgNamespace "rect" attrs $ return ()
    return ()

showFacet :: MonadWidget t m => Int -> Int -> Dynamic t Facet -> m ()
showFacet row col dFacet = do
    showFacetSquare row col 0.0 $ constDyn "black"
    showFacetSquare row col 0.05 =<< mapDyn (show.val) dFacet

showArrow :: MonadWidget t m => Rotation -> Dynamic t Facet -> m (Event t Action)
showArrow rotation dFacet = do
       let pointsString = if rotation == CW 
                          then "0.7,0.3 0.7,0.7 2.4,0.5"
                          else "0.3,0.7 0.7,0.7 0.5,2.4"
       (el,_) <- elDynAttrNS' svgNamespace "polygon" (constDyn $  "fill" =: "grey" 
                                                               <> "points" =: pointsString) $ return ()
       return $ attachWith (\a _ -> RotateFace CCW a)  (current dFacet) $ domEvent Click el


showArrows :: MonadWidget t m => Dynamic t Facet -> m (Event t Action)
showArrows dFacet = do
    arrowEventCW <- showArrow CW dFacet 
    let rotationEventCW = attachWith (\a _ -> RotateFace CW a)  (current dFacet) arrowEventCW
    arrowEventCCW <- showArrow CCW dFacet 
    let rotationEventCCW = attachWith (\a _ -> RotateFace CCW a)  (current dFacet) arrowEventCCW
    return $ leftmost [rotationEventCW, rotationEventCCW]

showFace :: MonadWidget t m => Dynamic t Facet -> m (Event t Action)
showFace upperLeft = do
    (_,ev) <- elDynAttrNS' svgNamespace "svg" 
                (constDyn $  "viewBox" =: "0 0 3 3 "
                          <> "width" =: show width
                          <> "height" =: show height)
                $ do showFacet 0 0 upperLeft 
                     showFacet 0 1 =<< mapDyn east upperLeft
         
                     upperRight <- mapDyn (east . east) upperLeft
                     showFacet 0 2 upperRight 
                     showFacet 1 2 =<< mapDyn east upperRight
         
                     lowerRight <- mapDyn (east . east) upperRight
                     showFacet 2 2 lowerRight 
                     showFacet 2 1 =<< mapDyn east lowerRight
         
                     lowerLeft <- mapDyn (east . east) lowerRight
                     showFacet 2 0 lowerLeft 
                     showFacet 1 0 =<< mapDyn east lowerLeft
         
                     center <- mapDyn (south . east) upperLeft
                     showFacet 1 1 center 

                     showArrows center
    return ev

floatLeft = "style" =: "float:left" 
clearLeft = "style" =: "clear:left" 

showCube :: MonadWidget t m => Dynamic t Facet -> m (Event t Action)
showCube cube = do

    let advanceSteps = [ west.north
                       , west . west . south
                       , north . east . east
                       , north . east . east
                       , north . east . east
                       , west . west . south 
                       ]

        advancer (prevMap, prevFace) step = 
            let newFace = step prevFace
                centerColor = val $ (south.south) newFace
            in (insert centerColor newFace prevMap, newFace) 

    faceMap <- mapDyn (\c -> fst $ foldl advancer (empty, c) advanceSteps) cube
    eventsWithKeys <- listWithKey faceMap $ const showFace
    return (switch $ (leftmost . elems) <$> current eventsWithKeys)


view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = showCube =<< mapDyn cube model

data Rotation = CCW | CW deriving (Ord, Eq)
data Action = RotateFace Rotation Facet 

data Model = Model { cube :: Facet 
                   , anchorCenter :: Vector3 Float
                   , northDirection :: Vector3 Float
                   }

targets :: Facet -> [FacetSig]
targets f = fmap signature [ (west.south) f, (east.east) f ]

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
        case action of
            RotateFace direction facet -> 
                 Model (rotateFace direction facet) (anchorCenter model) (northDirection model)

initModel = Model mkCube (Vector3 0.0 0.0 1.0) (Vector3 0.0 1.0 0.0)

main = mainWidget $ do 
           rec
               selectEvent <- view model 
               model <- foldDyn Main.update initModel selectEvent
           return ()
