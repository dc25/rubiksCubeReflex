{-# LANGUAGE RecursiveDo #-}
import Prelude(Eq,Ord(compare),Show,Enum,Num,Bool(True,False),Float,Int,String,Maybe(Just),const,show,fromIntegral,replicate,concat,zip,zipWith,sum,take,not,pi,sin,cos,head,(.),($),(+),(-),(*),(/),(==),(<),(>),(<$>),(!!),(++))
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, elem, find)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Matrix (Matrix, fromLists, toLists, multStd2, multStd)
import Data.Monoid ((<>))
import Data.Function 
import Control.Monad.Reader

data Color = Red | Green | Blue | Yellow | Orange | Purple | Black deriving (Show,Eq,Ord,Enum)

type Vector a = [a]

cross :: Num a => Vector a -> Vector a -> Vector a
cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                              ,z0*x1 - x0*z1
                              ,x0*y1 - y0*x1 ]

dot :: Num a => Vector a -> Vector a -> a
dot v0 v1 = sum $ zipWith (*) v0 v1

vMinus :: Num a => Vector a -> Vector a -> Vector a
vMinus = zipWith (-) 

data DNode a = DNode { north :: DNode a
                     , west  :: DNode a
                     , south :: DNode a
                     , east  :: DNode a
                     , color   :: a
                     , index :: Int
                     }

type Signature a = (a, Int)
type FacetSig = Signature Color

signature :: DNode a -> Signature a
signature dn = (color dn, index dn)

instance Eq a => Eq (DNode a) where 
    d0 == d1 = signature d0 == signature d1 

instance Ord a => Ord (DNode a) where 
    compare d0 d1 = compare (signature d0) (signature d1) 

type Facet = DNode Color
type Edge = (Facet,Facet,Facet)
type OrientedCube = (Facet, Matrix Float)
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
--- |     N      |     N      |     N      |     N      |
--- |            |            |            |            |
--- |W  yellow  E|W   red    E|W   green  E|W   blue    E|
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
        (nOrange, wOrange, sOrange,  eOrange) = mkFace sYellow   sBlue     sGreen    sRed     Orange
        (_,cube,_) = nPurple
    in south cube

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
rotateFace rotation face cube = 
        let advancer = if rotation == CW then south.west.west else east.north.east
        in copyWithRotation (getRotationMap advancer face) cube

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

-- pain point; what would it take to memoize some of these results?
transformPoints :: Matrix Float -> [(Float,Float)] -> [(Float,Float)]
transformPoints transform points = 
    let points4d = fmap (\(x,y) -> fromLists[[x,y,0,1]]) points 
        result4d = fmap (\p -> toLists $ multStd2 p transform) points4d
        result2d = fmap (\[[x,y,z,w]] -> (x/w,y/w)) result4d

    in result2d

pointsToString :: [(Float,Float)] -> String
pointsToString points = concat $ fmap (\(x,y) -> show x ++ ", " ++ show y ++ " ") points

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t FaceViewKit -> m ()
showFacetSquare x y margin dFaceViewKit = do
    let x0 = fromIntegral x + margin
        y0 = fromIntegral y + margin
        x1 = x0 + 1 - 2 * margin
        y1 = y0 + 1 - 2 * margin
        points = [(x0,y0),(x0,y1),(x1,y1),(x1,y0)]
    dAttrs <- mapDyn (\fvk -> "fill" =: (show.color.face) fvk  <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return ()

showFacet :: MonadWidget t m => Int -> Int -> Dynamic t FaceViewKit -> m ()
showFacet x y dFaceViewKit = do
    showFacetSquare x y 0 =<< mapDyn (changeViewKitColor Black) dFaceViewKit 
    showFacetSquare x y 0.05 dFaceViewKit

showArrow :: MonadWidget t m => Rotation -> [(Float,Float)] -> Dynamic t FaceViewKit -> m (Event t ())
showArrow rotation cwPoints dFaceViewKit = do
    let points = if rotation == CW then cwPoints else fmap (\(a,b) -> (b,a)) cwPoints

    dAttrs <- mapDyn (\fvk -> "fill" =: "grey" <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return $ domEvent Click el

showArrowSet :: MonadWidget t m => Rotation -> Dynamic t FaceViewKit -> m (Event t ())
showArrowSet rotation dFaceViewKit = do
    let hw = 0.35
        base = -0.3
        length = 0.7

        cwPoints0 = [(0.5 - hw,   1.5 + base), (0.5 + hw,   1.5 + base), (0.5,             1.5+base+length)]
        cwPoints1 = [(1.5 + base, 2.5 - hw),   (1.5 + base, 2.5 + hw),   (1.5+base+length, 2.5)]

    ev0 <- showArrow rotation cwPoints0 dFaceViewKit
    ev1 <- showArrow rotation cwPoints1 dFaceViewKit
    return $ leftmost [ev0, ev1]


showArrows :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showArrows dFaceViewKit = do
    dFacet <- mapDyn face dFaceViewKit  
    arrowEventCW <- showArrowSet CW dFaceViewKit
    let rotationEventCW = attachWith (\a _ -> RotateFace CW a)  (current dFacet) arrowEventCW
    arrowEventCCW <- showArrowSet CCW dFaceViewKit
    let rotationEventCCW = attachWith (\a _ -> RotateFace CCW a)  (current dFacet) arrowEventCCW
    return $ leftmost [rotationEventCW, rotationEventCCW]

-- pain point 
-- How do I do these repeated "east" operations as a fold (or something )
showFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showFace center = do  
    showFacet 1 1 center 

    right <- mapDyn (updateViewKit east) center
    showFacet 2 1 right  

    lowerRight <- mapDyn (updateViewKit east) right
    showFacet 2 0 lowerRight 

    lower <- mapDyn (updateViewKit east) lowerRight
    showFacet 1 0 lower

    lowerLeft <- mapDyn (updateViewKit east) lower
    showFacet 0 0 lowerLeft 

    left <- mapDyn (updateViewKit east) lowerLeft
    showFacet 0 1  left

    upperLeft <- mapDyn (updateViewKit east) left
    showFacet 0 2 upperLeft 

    upper <- mapDyn (updateViewKit east) upperLeft
    showFacet 1 2 upper

    upperRight <- mapDyn (updateViewKit east) upper
    showFacet 2 2 upperRight 

    showArrows center

updateViewKit :: (Facet->Facet) -> FaceViewKit -> FaceViewKit
updateViewKit advancer prevViewKit = prevViewKit { face = advancer $ face prevViewKit }

changeViewKitColor :: Color -> FaceViewKit -> FaceViewKit
changeViewKitColor newColor prevViewKit = 
    let prevFace = face prevViewKit
        newFace = prevFace {color=newColor}
    in prevViewKit {face = newFace}

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
        -- backface elimination follows.
    let threeUntransformedPoints = fromLists [ [0,0,0,1]   -- lower left corner of original face
                                             , [3,0,0,1]   -- lower right corner of original face
                                             , [0,3,0,1] ] -- upper left corner of original face

        threeTransformedPoints = toLists $ threeUntransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ threeTransformedPoints !! 0
        pt30 = take 3 $ threeTransformedPoints !! 1
        pt03 = take 3 $ threeTransformedPoints !! 2

        tVec_30_00 = pt30 `vMinus` pt00  -- vector from lower right to lower left
        tVec_03_00 = pt03 `vMinus` pt00  -- vector from upper left to lower left

        perpendicular = tVec_30_00 `cross` tVec_03_00  -- cross to get perpendicular pointing out from face.
        cameraToPlane = pt00 `vMinus` viewPoint

        -- perpendicular always points out from surface of cube.
        -- camera vector points in to surface of cube.
        -- For face to be visible, camera vector and perpendicular 
        -- should be opposed to each other. 
    in cameraToPlane `dot` perpendicular < 0

makeViewKit :: Facet -> Matrix Float -> Int -> FaceViewKit
makeViewKit facet orientation turns = 
    let 
        scale2d = 1/3  -- scale from 3x3 square face to 1x1 square face.
        scale2dMatrix = fromLists [ [scale2d, 0,       0,       0]
                                  , [0,       scale2d, 0,       0]
                                  , [0,       0,       0,       0]
                                  , [0,       0,       0,       1] ]

        trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = fromLists [ [1,       0,       0,       0]
                                  , [0,       1,       0,       0]
                                  , [0,       0,       1,       0]
                                  , [trans2d, trans2d, 0,       1] ]

        -- account for turns away from starting position
        c = cos (pi * fromIntegral turns / 2.0)
        s = sin (pi * fromIntegral turns / 2.0)

        rotation = fromLists [[ c, s, 0, 0]
                             ,[-s, c, 0, 0]
                             ,[ 0, 0, 1, 0]
                             ,[ 0, 0, 0, 1]]

        assemblies = fromList
                        [ ( Purple, 
                            fromLists [[ 1,   0,   0,   0]
                                      ,[ 0,   1,   0,   0]
                                      ,[ 0,   0,   1,   0]
                                      ,[ 0,   0,   0.5, 1] ]
                          )  

                        , ( Red,
                            fromLists [[ 0,   1,   0,   0]
                                      ,[ 0,   0,   1,   0]
                                      ,[ 1,   0,   0,   0]
                                      ,[ 0.5, 0,   0,   1] ]
                          )  

                        , ( Green,
                            fromLists [[-1,   0,   0,   0]
                                      ,[ 0,   0,   1,   0]
                                      ,[ 0,   1,   0,   0]
                                      ,[ 0,   0.5, 0,   1] ]
                          )  

                        , ( Blue,
                            fromLists [[ 0,  -1,   0,   0]
                                      ,[ 0,   0,   1,   0]
                                      ,[-1,   0,   0,   0]
                                      ,[-0.5, 0,   0,   1] ]
                          )  

                        , ( Yellow,
                            fromLists [[ 1,   0,   0,   0]
                                      ,[ 0,   0,   1,   0]
                                      ,[ 0,  -1,   0,   0]
                                      ,[ 0,  -0.5, 0,   1] ]
                          )  

                        , ( Orange,
                            fromLists [[ 1,   0,   0,   0]
                                      ,[ 0,  -1,   0,   0]
                                      ,[ 0,   0,  -1,   0]
                                      ,[ 0,   0,  -0.5, 1] ]
                          )
                        ]

        Just assemble = lookup (color facet) assemblies 

        scale3d = 1/2  -- scale down to fit in camera space
        scale3dMatrix = fromLists [ [scale3d, 0,       0,       0]
                                  , [0,       scale3d, 0,       0]
                                  , [0,       0,       scale3d, 0]
                                  , [0,       0,       0,       1] ]

        modelTransform =            scale2dMatrix 
                         `multStd2` trans2dMatrix 
                         `multStd2` rotation
                         `multStd2` assemble
                         `multStd2` scale3dMatrix 
                         `multStd2` orientation

        isFacingCamera = facingCamera [0,0,-1] modelTransform

        -- translate model to (0,0,1) for perspective viewing
        perspectivePrep = fromLists [ [1, 0, 0, 0]
                                    , [0, 1, 0, 0]
                                    , [0, 0, 1, 0]
                                    , [0, 0, 1, 1] ]

        -- perspective transformation 
        perspective     = fromLists [ [1, 0, 0, 0]
                                    , [0, 1, 0, 0]
                                    , [0, 0, 1, 1]
                                    , [0, 0, 0, 0] ]

        viewTransform =            modelTransform
                        `multStd2` perspectivePrep
                        `multStd2` perspective

    in FaceViewKit facet isFacingCamera viewTransform 

kitmapUpdate :: Matrix Float -> Map Color (Facet, Int) -> Map Color FaceViewKit -> Color -> Map Color FaceViewKit
kitmapUpdate orientation faceMap prevMap faceColor = 
    let Just (face, turns) = lookup faceColor faceMap 
        updatedViewKit = makeViewKit face orientation turns
        updatedMap = if isVisible updatedViewKit 
                     then insert faceColor updatedViewKit prevMap
                     else prevMap
    in updatedMap
    
-- pain point : Missing comma in red matrix caused difficult to diagnose
-- failure.
prepareFaceViews :: OrientedCube -> Map Color FaceViewKit
prepareFaceViews orientedCube@(startingFace, cubeOrientation) = 
    let advanceSteps :: Map Color (Color, [Facet -> Facet])
        advanceSteps = 
            fromList [ (Purple, (Red,    [east, south, west, north]))
                     , (Red,    (Orange, [south, west, north, east]))
                     , (Orange, (Yellow, [north, east, south, west]))
                     , (Yellow, (Blue,   [west, north, east, south]))
                     , (Blue,   (Green,  [west, north, east, south]))
                     , (Green,  (Purple, [north, east, south, west]))
                     ]

        getTurns face = 
            let Just (nextColor, advancers) = lookup (color face) advanceSteps
                colorChecker (advance,_) = (nextColor == (color.south.north.advance) face) 
                Just (advance,turns) = find colorChecker $ zip advancers [0..]
                nextFace = (south.north.advance) face
            in (nextColor, (face, turns)):getTurns nextFace  -- face contains color; is color as index necessary.

        facesWithTurns = fromList (take 6 $ getTurns startingFace) -- 6 faces.

        faceViewKits = foldl (kitmapUpdate cubeOrientation facesWithTurns) empty [Red .. Purple]
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
        [ex,ey,ez]    = nv `cross` ac
        orientation = [[ex,  ey,  ez, 0]
                      ,[nx,  ny,  nz, 0]
                      ,[ax,  ay,  az, 0]
                      ,[ 0,   0,   0, 1] ]
    in (cube model, fromLists orientation)

fps = "style" =: "float:left;padding:10px" 
cps = "style" =: "float:clear" 

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = 
    el "div" $ do
        leftEv <- fmap (const $ NudgeCube Left) <$> elAttr "div" fps (button "left" )
        rightEv <- fmap (const $ NudgeCube Right) <$> elAttr "div" fps ( button "right" )
        upEv <- fmap (const $ NudgeCube Up) <$>  elAttr "div" fps ( button "up")
        downEv <- fmap (const $ NudgeCube Down) <$> elAttr "div" fps (button "down" )
        (_,ev) <- elDynAttrNS' svgNamespace "svg" 
                    (constDyn $  "viewBox" =: "-0.48 -0.48 0.96 0.96"
                              <> "width" =: "575"
                              <> "height" =: "575") $ do
            orientedCube <- mapDyn orientCube model
            viewOrientedCube orientedCube
        return $ leftmost [ev, leftEv, rightEv, upEv, downEv]

data Rotation = CCW | CW deriving Eq
data Direction = Up | Down | Left | Right 

data Action = NudgeCube Direction | RotateFace Rotation Facet 

data Model = Model { cube :: Facet 
                   , perpendicular :: Vector Float
                   , northDirection :: Vector Float
                   }

targets :: Facet -> [FacetSig]
targets f = fmap signature [ (west.south) f, (east.east) f ]

applyRotation :: Matrix Float -> [Float] -> [Float]
applyRotation rotationMatrix  vec = 
    let vecMat = fromLists [vec]
        vecMatResult = vecMat `multStd2` rotationMatrix
    in head $ toLists vecMatResult

rotateModel rotationMatrix model = 
    model { perpendicular = applyRotation rotationMatrix $ perpendicular model,
            northDirection = applyRotation rotationMatrix $ northDirection model
          }

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
        case action of
            RotateFace rotation facet -> 
                 model { cube = rotateFace rotation facet $ cube model }
            NudgeCube direction -> 
                -- pain point : do I pay for making these limited scope?   
                let step = pi/20
                    c = cos step
                    s = sin step

                    -- left and right hold y axis const, rotate x,z
                    leftRotation =  fromLists [ [ c, 0,-s ]
                                              , [ 0, 1, 0 ]
                                              , [ s, 0, c ] ]

                    rightRotation = fromLists [ [ c, 0, s ]
                                              , [ 0, 1, 0 ]
                                              , [-s, 0, c ] ]

                    -- up and down hold x axis const, rotate y,z
                    upRotation =    fromLists [ [ 1, 0, 0 ]
                                              , [ 0, c,-s ]
                                              , [ 0, s, c ] ]

                    downRotation =  fromLists [ [ 1, 0, 0 ]
                                              , [ 0, c, s ]
                                              , [ 0,-s, c ] ]

                in case direction of
                       Left ->  rotateModel leftRotation model
                       Right -> rotateModel rightRotation model
                       Up ->    rotateModel upRotation model
                       Down ->  rotateModel downRotation model
 
initModel = Model mkCube [0,0,1]  [0,1,0] 

main = mainWidget $ do 
           rec
               selectEvent <- view model
               model <- foldDyn Main.update initModel selectEvent
           return ()
