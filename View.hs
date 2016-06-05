module View (view) where

import Prelude(Int,Bool(True,False),Float,String,fromIntegral,concatMap,zipWith,sum,take,pi,not,const,show,(<$>),($),(.),(*),(/),(+),(-),(++),(==),(!!),(<))

import Reflex.Dom ( MonadWidget ,Dynamic ,Event ,EventName(Click) ,attachWith ,button ,constDyn ,current ,domEvent ,el ,elAttr ,elDynAttrNS' ,leftmost ,listWithKey ,mapDyn ,switch ,(=:) ,(&))

import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, scanl,head)
import Data.Maybe (Maybe(Just))
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid ((<>))
import Control.Monad(sequence,fmap,return,(>>=),(=<<))

import Matrices
import Rotation
import Direction
import Action
import Cube
import Model

data FaceViewKit = FaceViewKit { face :: Facet
                               , isVisible :: Bool
                               , transform :: Matrix Float
                               }

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points = 
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

pointsToString :: [(Float,Float)] -> String
pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 

showFacetRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetRectangle x0 y0 x1 y1 dFaceViewKit = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
    dAttrs <- mapDyn (\fvk -> "fill" =: (show.color.face) fvk  <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return dFaceViewKit

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetSquare x y margin dFaceViewKit = do
    let x0 = fromIntegral x + margin
        y0 = fromIntegral y + margin
        x1 = x0 + 1 - 2 * margin
        y1 = y0 + 1 - 2 * margin
    showFacetRectangle x0 y0 x1 y1 dFaceViewKit

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
              >>= showAndAdvance 0 1 east            -- left
              >>= showAndAdvance 0 2 east            -- upper left
              >>= showAndAdvance 1 2 east            -- upper
              >>= showAndAdvance 2 2 east            -- upper right
              >>= showAndAdvance 2 1 east            -- right
              >>= showAndAdvance 2 0 east            -- lower right
              >>= showAndAdvance 1 0 south           -- lower

    showFacet 1 1 center          
    showArrows center [0,1,2,3] [0,1,2,3]

showInside :: MonadWidget t m => Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showInside dFaceViewKit = showFacetRectangle 0 0 3 3 =<< mapDyn (changeViewKitColor Brown) dFaceViewKit 

showUpperMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showUpperMiddleFace upperLeft = do  
    upper <-      showAndAdvance 0 2 east upperLeft  -- upper left
              >>= showFacet 1 2                      -- upper

    center <-     advance south upper                -- upper (already shown)
    _      <-     advance east upper 
              >>= showFacet 2 2                      -- upper right

    showArrows center [2,3] [2]

showLowerMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showLowerMiddleFace lowerLeft = do  
    _ <-          showAndAdvance 0 0 east lowerLeft  -- lower left
              >>= showFacet 0 1                      -- left 

    center <-     advance south lowerLeft            -- lower left (already shown)
              >>= showAndAdvance 1 0 west            -- lower
              >>= showAndAdvance 2 0 south           -- lower right
              >>= showAndAdvance 2 1 south           -- right (advance to center)

    showFacet 1 1 center                             -- center
    showArrows center [0,1] [0,1,3]

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
    let cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                                      ,z0*x1 - x0*z1
                                      ,x0*y1 - y0*x1 ]

        dot v0 v1 = sum $ zipWith (*) v0 v1

        vMinus = zipWith (-) 

        threeUntransformedPoints = fromLists [ [0,0,0,1]   -- lower left corner of original face
                                             , [3,0,0,1]   -- lower right corner of original face
                                             , [0,3,0,1] ] -- upper left corner of original face

        threeTransformedPoints = toLists $ threeUntransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ head threeTransformedPoints 
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

makeViewKit :: Model -> Facet -> Bool -> Float -> FaceViewKit
makeViewKit (Model topFace orientation twist) lowerLeft withTwist offset = 
    let scale2dMatrix = scaleMatrix (1/3) -- scale from 3x3 square face to 1x1 square face.

        trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = translationMatrix (trans2d,trans2d,0)

        faceColor = (color.south.south) lowerLeft 
        topColor = color topFace

        -- If face A is the face being rendered and face B is the face that
        -- cooresponds to face A if the top face were Purple, then this map
        -- contains the number of turns to get the frame for face A
        -- to match the frame for face B.
        -- This is necessary because lowerLeft is determined "as if" the
        -- Purple face is at the top of the model ( as it initially is ).
        
        turns = fromList [( (Purple, Yellow), 0)
                         ,( (Purple, Red),    0)
                         ,( (Purple, Purple), 0)
                         ,( (Purple, Green),  0)
                         ,( (Purple, Blue),   0)
                         ,( (Purple, Orange), 0)

                         ,( (Blue,   Blue),   0)
                         ,( (Green,  Green),  0)
                         ,( (Red,    Red),    0)
                         ,( (Yellow, Yellow), 0)
                         ,( (Orange, Orange), 0)

                         ,( (Blue,   Yellow), 1)
                         ,( (Green,  Blue),   1)
                         ,( (Red,    Green),  1)
                         ,( (Yellow, Red),    1)

                         ,( (Blue,   Red),    2)
                         ,( (Green,  Yellow), 2)
                         ,( (Red,    Blue),   2)
                         ,( (Yellow, Green),  2)

                         ,( (Blue,   Green),  3)
                         ,( (Green,  Red),    3)
                         ,( (Red,    Yellow), 3)
                         ,( (Yellow, Blue),   3)

                         ,( (Green,  Purple), 0)
                         ,( (Blue,   Purple), 1)
                         ,( (Yellow, Purple), 2)
                         ,( (Red,    Purple), 3)

                         ,( (Yellow, Orange), 0)
                         ,( (Blue,   Orange), 1)
                         ,( (Green,  Orange), 2)
                         ,( (Red,    Orange), 3) 

                         ,( (Orange, Yellow), 2)
                         ,( (Orange, Red),    2)
                         ,( (Orange, Green),  2)
                         ,( (Orange, Blue),   2) 
                         
                         ,( (Orange, Purple), 0) ]

        Just turnCount = lookup (topColor, faceColor) turns 
        turnMatrix = xyRotationMatrix (fromIntegral turnCount * pi / 2)
        offsetMatrix = translationMatrix (0,0,offset)

        -- Rotate face into position .  Rotations and inverses specified.
        assemblies = fromList
                        [ ( Purple
                          , (  []
                            ,  []
                            )
                          )  

                        , ( Yellow
                          , ([ yzRotationMatrix (pi/2) ]
                            ,[ yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Red
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (pi/2) ]
                            ,[ xyRotationMatrix (-pi/2)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Green
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix pi ]
                            ,[ xyRotationMatrix (-pi)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Blue
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (-pi/2) ]
                            ,[ xyRotationMatrix (pi/2)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Orange
                          , ([ yzRotationMatrix pi ]
                            ,[ yzRotationMatrix (-pi) ]
                            )
                          )
                        ]

        Just (assembleMatricies,_) = lookup faceColor assemblies 
        Just (postTwist,preTwist) = lookup topColor assemblies 

        twistMatricies = 
            if withTwist 
            then preTwist ++
                 [ xyRotationMatrix (2*pi * twist/360) ] ++
                 postTwist
            else []

        -- scale down to fit in camera space
        scale3dMatrix = scaleMatrix (1/2)

        modelTransformations = [ scale2dMatrix
                               , trans2dMatrix 
                               , turnMatrix 
                               , offsetMatrix
                               ] ++ 
                               assembleMatricies ++ -- may be 0,1 or 2 matricies
                               twistMatricies ++ -- may be 0 or up to 5 ( I think ) matricies
                               [ scale3dMatrix,
                                 orientation
                               ]

        -- combine to single transform from 2d to 3d
        modelTransform =  foldl multStd2 identityMatrix modelTransformations

        -- backface elimination
        isFacingCamera = facingCamera [0,0,-1] modelTransform

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspectivePrepMatrix
                        `multStd2` perspectiveMatrix

    in FaceViewKit lowerLeft isFacingCamera viewTransform 

kitmapUpdate :: Model -> Bool -> Float -> Map Color FaceViewKit -> Facet -> Map Color FaceViewKit
kitmapUpdate model withTwist offset prevMap lowerLeft = 
    let updatedViewKit = makeViewKit model lowerLeft withTwist offset
    in  if isVisible updatedViewKit 
        then insert ((color.south.south) lowerLeft) updatedViewKit prevMap
        else prevMap

withTwist = True -- just a constant for readability

topView :: Model -> Map Color FaceViewKit
topView model@(Model center _ _)  =
    foldl (kitmapUpdate model withTwist (1.0/2.0)) empty [getLowerLeft center]

bottomInsideView :: Model -> Map Color FaceViewKit
bottomInsideView model@(Model center _ _)  =
    if twist model == 0 
    then empty
    else foldl (kitmapUpdate model withTwist (-1.0/6.0)) empty [(west.south.west.west.south.west.getLowerLeft) center]

topInsideView :: Model -> Map Color FaceViewKit
topInsideView model@(Model center _ _)  =
    if twist model == 0 
    then empty
    else foldl (kitmapUpdate model (not withTwist) (1.0/6.0)) empty [getLowerLeft center]

upperMiddleView :: Model -> Map Color FaceViewKit
upperMiddleView model@(Model center _ _)   =
    let upperLeft = (west.getLowerLeft) center
        advancers = [ north.east.east
                    , north.east.east
                    , north.east.east
                    ]
        upperLefts = scanl (&) upperLeft advancers  -- get upper left corners of all faces
    in foldl (kitmapUpdate model withTwist 0.5) empty upperLefts

bottomView :: Model -> Map Color FaceViewKit
bottomView model@(Model center _ _)  =
    foldl (kitmapUpdate model (not withTwist) 0.5) empty [(west.south.west.west.south.west.getLowerLeft) center]

lowerMiddleView :: Model -> Map Color FaceViewKit
lowerMiddleView model@(Model center _ _)  =
    let lowerLeft = (west.south.west.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
        lowerLefts = scanl (&) lowerLeft advancers  -- get lower left corners of all faces
    in foldl (kitmapUpdate model (not withTwist) 0.5) empty lowerLefts

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

-- pain point : How do I make the order of display conditional
viewModel :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
viewModel model = do
    bottomMap <-                    mapDyn bottomView model
    lowerMiddleMap <-               mapDyn lowerMiddleView model
    bottomInsideMap <-              mapDyn bottomInsideView model
    topInsideMap <-                 mapDyn topInsideView model
    upperMiddleMap <-               mapDyn upperMiddleView model
    topMap <-                       mapDyn topView model

    bottomEventsWithKeys <-         listWithKey bottomMap $ const showFace
    lowerMiddleEventsWithKeys <-    listWithKey lowerMiddleMap $ const showLowerMiddleFace
    _ <-                            listWithKey bottomInsideMap $ const showInside
    _ <-                            listWithKey topInsideMap $ const showInside
    upperMiddleEventsWithKeys <-    listWithKey upperMiddleMap $ const showUpperMiddleFace
    topEventsWithKeys <-            listWithKey topMap $ const showFace

    let topEvent = switch $ (leftmost . elems) <$> current topEventsWithKeys
        bottomEvent = switch $ (leftmost . elems) <$> current bottomEventsWithKeys
        lowerMiddleEvent = switch $ (leftmost . elems) <$> current lowerMiddleEventsWithKeys
        upperMiddleEvent = switch $ (leftmost . elems) <$> current upperMiddleEventsWithKeys
    return $ leftmost [topEvent, lowerMiddleEvent, upperMiddleEvent, bottomEvent]

fps = "style" =: "float:left;padding:10px" 
cps = "style" =: "float:clear" 

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = 
    el "div" $ do
        leftEv <-    fmap (const $ NudgeCube Left)  <$> elAttr "div" fps (button "left")
        rightEv <-   fmap (const $ NudgeCube Right) <$> elAttr "div" fps (button "right")
        upEv <-      fmap (const $ NudgeCube Up)    <$> elAttr "div" fps (button "up")
        downEv <-    fmap (const $ NudgeCube Down)  <$> elAttr "div" fps (button "down")
        (_,ev) <-    elDynAttrNS' svgNamespace "svg" 
                       (constDyn $  "viewBox" =: "-0.48 -0.48 0.96 0.96"
                                 <> "width" =: "575"
                                 <> "height" =: "575") $ viewModel model
        return $ leftmost [ev, leftEv, rightEv, upEv, downEv]

