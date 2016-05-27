{-# LANGUAGE RecursiveDo #-}
import Prelude(Eq,Ord(compare),Show,Enum,Num,Bool(True,False),Float,Int,String,Maybe(Just),fst,const,show,fromIntegral,replicate,concat,zipWith,sum,take,not,(.),($),(+),(-),(*),(/),(==),(<),(>),(<$>),(!!),(++))
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, elem)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Matrix (Matrix, fromLists, toLists, multStd2, multStd)
import Data.Monoid ((<>))
import Control.Monad.Reader

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show,Eq,Ord,Enum)

type Vector a = [a]

cross :: Num a => Vector a -> Vector a -> Vector a
cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                              ,z0*x1 - x0*z1
                              ,x0*y1 - y0*x1
                              ]

dot :: Num a => Vector a -> Vector a -> a
dot v0 v1 = sum $ zipWith (*) v0 v1

vMinus :: Num a => Vector a -> Vector a -> Vector a
vMinus = zipWith (-) 

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
type Orientation = Matrix Float
type OrientedCube = (Facet, Orientation)
data FaceViewKit = FaceViewKit { face :: Facet
                               , isVisible :: Bool
                               , transform :: Matrix Float
                               }

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

width = 300
height = 300

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

showFacet :: MonadWidget t m => Int -> Int -> Dynamic t FaceViewKit -> m ()
showFacet row col dFaceViewKit = do
    return ()
    -- showFacetSquare row col 0.0 $ constDyn "black"
    -- showFacetSquare row col 0.05 =<< mapDyn (show.val.face) dFaceViewKit

transformPoints :: [(Float,Float)] -> Matrix Float -> [(Float,Float)]
transformPoints points transform = 
    let points4d = fmap (\(x,y) -> fromLists[[x,y,0.0,1.0]]) points 
        result4d = fmap (\p -> toLists $ multStd2 p transform) points4d
        result2d = fmap (\[[x,y,z,w]] -> (x/w,y/w)) result4d

    in result2d

showArrow :: MonadWidget t m => Rotation -> Dynamic t FaceViewKit -> m (Event t ())
showArrow rotation dFaceViewKit = do
    let points = if rotation == CW 
                 then [(0.7,0.3), (0.7,0.7), (2.4,0.5)]
                 else [(0.3,0.7), (0.7,0.7), (0.5,2.4)]
    dTransform <- mapDyn transform dFaceViewKit
    dTransformedPoints <- mapDyn (transformPoints points) dTransform
    dPointsString <- mapDyn (concat . fmap (\(x,y) -> show x ++ ", " ++ show y ++ " ")) dTransformedPoints
    dAttrs <- mapDyn (\ps -> "fill" =: "grey"  <> "points" =: ps) dPointsString
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return $ domEvent Click el


showArrows :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showArrows dFaceViewKit = do
    dFacet <- mapDyn face dFaceViewKit
    arrowEventCW <- showArrow CW dFaceViewKit
    let rotationEventCW = attachWith (\a _ -> RotateFace CW a)  (current dFacet) arrowEventCW
    arrowEventCCW <- showArrow CCW dFaceViewKit
    let rotationEventCCW = attachWith (\a _ -> RotateFace CCW a)  (current dFacet) arrowEventCCW
    return $ leftmost [rotationEventCW, rotationEventCCW]

showFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showFace upperLeft = do
    (_,ev) <- elDynAttrNS' svgNamespace "svg" 
                (constDyn $  "viewBox" =: "-1.0 -1.0 2.0 2.0"
                          <> "width" =: show width
                          <> "height" =: show height)
                $ do  -- maybe could use a fold or something for this.
                     showFacet 0 0 upperLeft  

                     upper <- mapDyn (updateViewKit east) upperLeft
                     showFacet 0 1  upper
         
                     upperRight <- mapDyn (updateViewKit east) upper
                     showFacet 0 2 upperRight 

                     right <- mapDyn (updateViewKit east) upperRight
                     showFacet 1 2 right
         
                     lowerRight <- mapDyn (updateViewKit east) right
                     showFacet 2 2 lowerRight 

                     lower <- mapDyn (updateViewKit east) lowerRight
                     showFacet 2 1 lower
         
                     lowerLeft <- mapDyn (updateViewKit east) lower
                     showFacet 2 0 lowerLeft 

                     left <- mapDyn (updateViewKit east) lowerLeft
                     showFacet 1 0 left
         
                     center <- mapDyn (updateViewKit south) left
                     showFacet 1 1 center 

                     showArrows center
    return ev

updateViewKit :: (Facet->Facet) -> FaceViewKit -> FaceViewKit
updateViewKit advancer prevViewKit = prevViewKit { face = advancer $ face prevViewKit }
    

makeViewKit :: Facet -> Orientation -> Matrix Float -> FaceViewKit
makeViewKit facet orientation assemble = 
    let 
        scale2d :: Float
        scale2d = 1.0/3.0
        scale2dMatrix = fromLists [ [scale2d, 0,       0,       0]
                                  , [0,       scale2d, 0,       0]
                                  , [0,       0,       1,       0]
                                  , [0,       0,       0,       1] 
                                  ]

        trans2d :: Float
        trans2d = -1.0/2.0
        trans2dMatrix = fromLists [ [1,       0,       0,       0]
                                  , [0,       1,       0,       0]
                                  , [0,       0,       1,       0]
                                  , [trans2d, trans2d, 0,       1] 
                                  ]

        scale3d :: Float
        scale3d = 1.0
        scale3dMatrix = fromLists [ [scale3d, 0,       0,       0]
                                  , [0,       scale3d, 0,       0]
                                  , [0,       0,       scale3d, 0]
                                  , [0,       0,       0,       1] 
                                  ]

        modelTransform =            scale2dMatrix 
                              `multStd2` trans2dMatrix 
                              `multStd2` assemble
                              `multStd2` orientation
                              `multStd2` scale3dMatrix


        transformRows = toLists modelTransform

        -- for backface elimination, perpendicular can be taken from third
        -- row ( where z axis projects to before applying any transforms) 
        -- and point on plane can be taken from row 4 ( where origin evaluates to ).
        perpendicular = take 3 $ transformRows !! 2
        pointOnPlane = take 3 $ transformRows !! 3

        viewPoint = [0.0,0.0,-1.0]
        cameraToPlane = pointOnPlane `vMinus` viewPoint

        -- perpendicular always points out from surface of cube.
        -- camera vector points in to surface of cube.
        -- For face to be visible, camera vector and perpendicular 
        -- should be opposed to each other. 
        isViewable = cameraToPlane `dot` perpendicular < 0.0

        perspectivePrep = fromLists [ [1,       0,       0,       0]
                                    , [0,       1,       0,       0]
                                    , [0,       0,       1,       0]
                                    , [0,       0,       1,       1] 
                                    ]

        perspective     = fromLists [ [1,       0,       0,       0]
                                    , [0,       1,       0,       0]
                                    , [0,       0,       0,       1]
                                    , [0,       0,       0,       1] 
                                    ]

        viewTransform =                  modelTransform
                              `multStd2` perspectivePrep
                              `multStd2` perspective

    in FaceViewKit facet isViewable viewTransform

kitmapUpdate :: Orientation -> (Map Color FaceViewKit, Facet) -> (Facet -> Facet, Matrix Float) -> (Map Color FaceViewKit, Facet) 
kitmapUpdate orientation (prevMap, prevFace) (advanceFunction, assemble)  = 
    let newFace = advanceFunction prevFace
        centerColor = val $ (south.south) newFace
        newViewKit = makeViewKit newFace orientation assemble
        newMap = if isVisible newViewKit 
                 then insert centerColor newViewKit prevMap
                 else prevMap
    in (newMap, newFace)
    
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

-- pain point : Missing comma in red matrix caused difficult to diagnose
-- failure.
prepareFaceViews :: OrientedCube -> Map Color FaceViewKit
prepareFaceViews orientedCube@(startingFace, cubeOrientation) = 
    let advanceSteps :: [(Facet -> Facet, Matrix Float)]
        advanceSteps = 
            [ (west . north,        
               fromLists [[ 1.0, 0.0, 0.0, 0.0 ]
                         ,[ 0.0, 1.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0, 1.0, 0.0 ]
                         ,[ 0.0, 0.0, 0.5, 1.0 ] 
                         ] )  -- purple / top

            , (west . west . south, 
               fromLists [[ 1.0, 0.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0, 1.0, 0.0 ]
                         ,[ 0.0,-1.0, 0.0, 0.0 ]
                         ,[ 0.0,-0.5, 0.0, 1.0 ] 
                         ] )  -- yellow / front

            , (north . east . east, 
               fromLists [[ 0.0, 1.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0, 1.0, 0.0 ]
                         ,[ 1.0, 0.0, 0.0, 0.0 ]
                         ,[ 0.5, 0.0, 0.0, 1.0 ] 
                         ] )  -- blue   / right

            , (north . east . east, 
               fromLists [[-1.0, 0.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0, 1.0, 0.0 ]
                         ,[ 0.0, 1.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.5, 0.0, 1.0 ] 
                         ] )  -- green  / back

            , (north . east . east, 
               fromLists [[ 0.0,-1.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0, 1.0, 0.0 ]
                         ,[-1.0, 0.0, 0.0, 0.0 ]
                         ,[-0.5, 0.0, 0.0, 1.0 ] 
                         ] )  -- red    / left

            , (west . west . south, 
               fromLists [[ 1.0, 0.0, 0.0, 0.0 ]
                         ,[ 0.0,-1.0, 0.0, 0.0 ]
                         ,[ 0.0, 0.0,-1.0, 0.0 ]
                         ,[ 0.0, 0.0,-0.5, 1.0 ] 
                         ] )  -- orange / bottom
            ]

        -- for each step, get a face, compute the view kit for that face,
        -- add the view kit to the map, using color as an index, replace
        -- the working face with the new face.
        (faceViewKits,_) = foldl (kitmapUpdate cubeOrientation) (empty, startingFace) advanceSteps
    in faceViewKits

viewOrientedCube :: MonadWidget t m => Dynamic t OrientedCube -> m (Event t Action)
viewOrientedCube orientedCube = do
    faceMap <- mapDyn prepareFaceViews orientedCube
    eventsWithKeys <- listWithKey faceMap $ const showFace
    return (switch $ (leftmost . elems) <$> current eventsWithKeys)

orientCube :: Model -> OrientedCube
orientCube model = 
        let ac@[ax,ay,az] = perpendicular model
            nv@[nx,ny,nz] = northDirection model
            ev@[ex,ey,ez] = nv `cross` ac
            orientation = [[ ex,  ey,  ez, 0.0]
                          ,[ nx,  ny,  nz, 0.0]
                          ,[ ax,  ay,  az, 0.0]
                          ,[0.0, 0.0, 0.0, 1.0]
                          ]
            in (cube model, fromLists orientation)

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = do
    orientedCube <- mapDyn orientCube model
    viewOrientedCube orientedCube

data Rotation = CCW | CW deriving (Ord, Eq)
data Action = RotateFace Rotation Facet 

data Model = Model { cube :: Facet 
                   , perpendicular :: Vector Float
                   , northDirection :: Vector Float
                   }

targets :: Facet -> [FacetSig]
targets f = fmap signature [ (west.south) f, (east.east) f ]

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
        case action of
            RotateFace direction facet -> 
                 Model (rotateFace direction facet) (perpendicular model) (northDirection model)

initModel = Model mkCube [0.0,0.0,1.0]  [0.0,1.0,0.0] 

main = mainWidget $ do 
           rec
               selectEvent <- view model
               model <- foldDyn Main.update initModel selectEvent
           return ()

