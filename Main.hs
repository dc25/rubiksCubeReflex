{-# LANGUAGE RecursiveDo #-}
import Prelude(Eq,Ord(compare),Show,Enum,Num,Bool(True,False),Float,Int,String,Maybe(Just),const,show,fromIntegral,replicate,concat,concatMap,zip,zipWith,sum,take,not,pi,sin,cos,head,(.),($),(+),(-),(*),(/),(==),(<),(>),(<$>),(!!),(++))
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, elem, find, scanl)
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
                     , color :: a
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
type AnnotatedCube = (Facet, Matrix Float)
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

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points = 
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

pointsToString :: [(Float,Float)] -> String
pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetSquare x y margin dFaceViewKit = do
    let x0 = fromIntegral x + margin
        y0 = fromIntegral y + margin
        x1 = x0 + 1 - 2 * margin
        y1 = y0 + 1 - 2 * margin
        points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
    dAttrs <- mapDyn (\fvk -> "fill" =: (show.color.face) fvk  <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return dFaceViewKit

changeViewKitColor :: Color -> FaceViewKit -> FaceViewKit
changeViewKitColor newColor prevViewKit = 
    let prevFace = face prevViewKit
        newFace = prevFace {color=newColor}
    in prevViewKit {face = newFace}

showFacet :: MonadWidget t m => Int -> Int -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacet x y dFaceViewKit = do
    showFacetSquare x y 0 =<< mapDyn (changeViewKitColor Black) dFaceViewKit 
    showFacetSquare x y 0.05 dFaceViewKit
    return dFaceViewKit

arrow :: Matrix Float
arrow = 
    let hw = 0.35
        base = 0.8
        length = 0.6
    in fromLists [[base, 0.5 - hw, 0, 1], [base, 0.5 + hw, 0, 1], [base-length, 0.5, 0, 1]]

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
scaleMatrix scale =
    fromLists  [[ scale,     0,     0,     0 ]
               ,[     0, scale,     0,     0 ]
               ,[     0,     0, scale,     0 ]
               ,[     0,     0,     0,     1 ]
               ]

arrowPoints :: (Rotation,Int) -> Matrix Float
arrowPoints (rotation,index) = 
    let cwRotations = [0, pi/2, pi, 3*pi/2]
        cwTranslations = [(0,0,0),(3,0,0),(3,3,0),(0,3,0)]

        cwTransformations = zipWith multStd2 
                              (fmap xyRotationMatrix cwRotations) 
                              (fmap translationMatrix cwTranslations)

        ccwRotations = [ pi, 3*pi/2, 0, pi/2 ]
        ccwTranslations = [(2,1,0),(2,2,0),(1,2,0),(1,1,0)]

        ccwTransformations = zipWith multStd2 
                              (fmap xyRotationMatrix ccwRotations) 
                              (fmap translationMatrix ccwTranslations)

        transformations = if rotation == CW then cwTransformations else ccwTransformations
        transform = transformations !! index
    in arrow `multStd2` transform

showArrow :: MonadWidget t m => Dynamic t FaceViewKit -> (Rotation, Int) -> m (Event t ())
showArrow dFaceViewKit arrowIndex = do
    let points = arrowPoints arrowIndex
    dAttrs <- mapDyn (\fvk -> "fill" =: "grey" <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points)) dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return $ domEvent Click el

arrowRotationEvent :: MonadWidget t m => Dynamic t FaceViewKit -> Rotation -> [Int] -> m (Event t Action)
arrowRotationEvent dFaceViewKit rotation cornerIndices = do
    dFacet <- mapDyn face dFaceViewKit  
    let arrowIndices = fmap ((,) rotation) cornerIndices
    arrowEvents <- sequence $ fmap (showArrow dFaceViewKit) arrowIndices
    let arrowEvent = leftmost arrowEvents
    return $ attachWith (\a _ -> RotateFace rotation a)  (current dFacet) arrowEvent

showArrows :: MonadWidget t m => Dynamic t FaceViewKit -> [Int] -> [Int] -> m (Event t Action)
showArrows dFaceViewKit cwCornerIndices ccwCornerIndices = do
    cwRotationEvent <- arrowRotationEvent dFaceViewKit CW cwCornerIndices 
    ccwRotationEvent <- arrowRotationEvent dFaceViewKit CCW ccwCornerIndices 
    return $ leftmost [cwRotationEvent, ccwRotationEvent]

advance :: MonadWidget t m => (Facet -> Facet) -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
advance adv dFaceViewKit = do
    let updateViewKit advancer prevViewKit = prevViewKit { face = advancer $ face prevViewKit }
    mapDyn (updateViewKit adv) dFaceViewKit

-- pain point how do I get the compiler to tell me what the type sig for
-- this function should be.
showAndAdvance :: MonadWidget t m => Int -> Int -> (Facet -> Facet) -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showAndAdvance x y adv dFaceViewKit = do
    showFacet x y dFaceViewKit
    advance adv dFaceViewKit

-- pain point 
-- How do I do these repeated "east" operations as a fold (or something )
showFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showFace lowerLeft = do  
    center <-     showAndAdvance 0 0 east lowerLeft  -- lower left
              >>= showAndAdvance 0 1 east   -- left
              >>= showAndAdvance 0 2 east   -- upper left
              >>= showAndAdvance 1 2 east   -- upper
              >>= showAndAdvance 2 2 east   -- upper right
              >>= showAndAdvance 2 1 east   -- right
              >>= showAndAdvance 2 0 east   -- lower right
              >>= showAndAdvance 1 0 south  -- lower

    showFacet 1 1 center          
    showArrows center [0,1,2,3] [0,1,2,3]

showUpperMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showUpperMiddleFace upperLeft = do  
    upper <-     showAndAdvance 0 2 east upperLeft -- upper left
             >>= showFacet 1 2           -- upper

    center <- advance south upper        -- upper (already shown)
    advance east upper >>= showFacet 2 2 -- upper right

    showArrows center [2,3] [2]

showLowerMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showLowerMiddleFace lowerLeft = do  
    _ <-         showAndAdvance 0 0 east lowerLeft  -- lower left
             >>= showFacet 0 1             -- left 

    center <-    advance south lowerLeft   -- lower left (already shown)
             >>= showAndAdvance 1 0 west   -- lower
             >>= showAndAdvance 2 0 south  -- lower right
             >>= showAndAdvance 2 1 south  -- right (advance to center)

    showFacet 1 1 center            -- center
    showArrows center [0,1] [0,1,3]

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
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

makeViewKit :: Facet -> Matrix Float -> FaceViewKit
makeViewKit facet orientation = 
    let 

        scale2d = 1/3  -- scale from 3x3 square face to 1x1 square face.
        scale2dMatrix = scaleMatrix scale2d

        trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = fromLists [ [1,       0,       0,       0]
                                  , [0,       1,       0,       0]
                                  , [0,       0,       1,       0]
                                  , [trans2d, trans2d, 0,       1] ]

        -- position face on cube.
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

        Just assemble = lookup ((color.south.south) facet) assemblies 

        -- scale down to fit in camera space
        scale3d = 1/2  
        scale3dMatrix = scaleMatrix scale3d

        -- combine to single transform from 2d to 3d
        modelTransform =            scale2dMatrix
                         `multStd2` trans2dMatrix 
                         `multStd2` assemble
                         `multStd2` scale3dMatrix
                         `multStd2` orientation

        -- backface elimination
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

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspectivePrep
                        `multStd2` perspective

    in FaceViewKit facet isFacingCamera viewTransform 

kitmapUpdate :: Matrix Float -> Map Color FaceViewKit -> Facet -> Map Color FaceViewKit
kitmapUpdate orientation prevMap lowerLeft = 
    let updatedViewKit = makeViewKit lowerLeft orientation 
        faceColor = (color.south.south) lowerLeft 
        updatedMap = if isVisible updatedViewKit 
                     then insert faceColor updatedViewKit prevMap
                     else prevMap
    in updatedMap
    
topView :: AnnotatedCube -> Map Color FaceViewKit
topView (center,cubeOrientation) =
    foldl (kitmapUpdate cubeOrientation) empty [getLowerLeft center]

bottomView :: AnnotatedCube -> Map Color FaceViewKit
bottomView (center,cubeOrientation) =
    foldl (kitmapUpdate cubeOrientation) empty [(west.south.west.west.south.west.getLowerLeft) center]

upperMiddleView :: AnnotatedCube -> Map Color FaceViewKit
upperMiddleView (center,cubeOrientation) =
    let upperLeft = (west.getLowerLeft) center
        advancers = [ north.east.east
                    , north.east.east
                    , north.east.east
                    ]
        upperLefts = scanl (&) upperLeft advancers  -- get upper left corners of all faces
    in foldl (kitmapUpdate cubeOrientation) empty upperLefts

lowerMiddleView :: AnnotatedCube -> Map Color FaceViewKit
lowerMiddleView (center,cubeOrientation) =
    let lowerLeft = (west.south.west.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
        lowerLefts = scanl (&) lowerLeft advancers  -- get lower left corners of all faces
    in foldl (kitmapUpdate cubeOrientation) empty lowerLefts

getLowerLeft :: Facet -> Facet
getLowerLeft centerFace =
    let centerFaceColor = color centerFace
        westFaceColor = (color.south.north.west) centerFace

        leftDirs = fromList [ ((Purple,  Blue),   west)
                            , ((Purple,  Yellow), north)
                            , ((Purple,  Red),    east)
                            , ((Purple,  Green),  south)
 
                            , ((Yellow,  Blue),   west)
                            , ((Yellow,  Orange), north)
                            , ((Yellow,  Red ),   east)
                            , ((Yellow,  Purple), south)
 
                            , ((Red,     Yellow), west)
                            , ((Red,     Orange), north)
                            , ((Red,     Green ), east)
                            , ((Red,     Purple), south)
 
                            , ((Green,   Red),    west)
                            , ((Green,   Orange), north)
                            , ((Green,   Blue ),  east)
                            , ((Green,   Purple), south)
 
                            , ((Blue,    Green),  west)
                            , ((Blue,    Orange), north)
                            , ((Blue,    Yellow ),east)
                            , ((Blue,    Purple), south)
 
                            , ((Orange,  Blue),   west)
                            , ((Orange,  Green),  north)
                            , ((Orange,  Red ),   east)
                            , ((Orange,  Yellow), south)
                            ]

        Just goLeft = lookup (centerFaceColor, westFaceColor) leftDirs
    in (west.goLeft) centerFace

viewAnnotatedCube :: MonadWidget t m => Dynamic t AnnotatedCube -> m (Event t Action)
viewAnnotatedCube annotatedCube = do
    topMap <- mapDyn topView annotatedCube
    topEventsWithKeys <- listWithKey topMap $ const showFace

    bottomMap <- mapDyn bottomView annotatedCube
    bottomEventsWithKeys <- listWithKey bottomMap $ const showFace

    upperMiddleMap <- mapDyn upperMiddleView annotatedCube
    upperMiddleEventsWithKeys <- listWithKey upperMiddleMap $ const showUpperMiddleFace

    lowerMiddleMap <- mapDyn lowerMiddleView annotatedCube
    lowerMiddleEventsWithKeys <- listWithKey lowerMiddleMap $ const showLowerMiddleFace

    let topEvent = switch $ (leftmost . elems) <$> current topEventsWithKeys
        bottomEvent = switch $ (leftmost . elems) <$> current bottomEventsWithKeys
        lowerMiddleEvent = switch $ (leftmost . elems) <$> current lowerMiddleEventsWithKeys
        upperMiddleEvent = switch $ (leftmost . elems) <$> current upperMiddleEventsWithKeys
    return $ leftmost [topEvent, lowerMiddleEvent, upperMiddleEvent, bottomEvent]

annotateCube :: Model -> AnnotatedCube
annotateCube model = 
    let ac@[ax,ay,az] = perpendicular model
        nv@[nx,ny,nz] = northDirection model
        [ex,ey,ez]    = nv `cross` ac
        orientation = fromLists [[ex,  ey,  ez, 0]
                                ,[nx,  ny,  nz, 0]
                                ,[ax,  ay,  az, 0]
                                ,[ 0,   0,   0, 1] ]
    in (cube model, orientation)

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
            annotatedCube <- mapDyn annotateCube model
            viewAnnotatedCube annotatedCube
        return $ leftmost [ev, leftEv, rightEv, upEv, downEv]

data Rotation = CCW | CW deriving Eq
data Direction = Up | Down | Left | Right 

data Action = NudgeCube Direction | RotateFace Rotation Facet 

data Model = Model { cube :: Facet 
                   , perpendicular :: Vector Float
                   , northDirection :: Vector Float
                   }

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
