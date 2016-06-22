module View (view, insideFacesCamera) where

import Prelude(Int,Bool(True,False),Float,String,fromIntegral,concatMap,zipWith,sum,take,pi,not,const,show,(<$>),($),(.),(*),(/),(+),(-),(++),(==),(/=),(!!),(<),(&&),(||))

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
import Color
import TwistMode
import Cube
import Model

viewScale = 500

data FaceViewKit = FaceViewKit { face :: Facet
                               , transform :: Matrix Float
                               }

type ViewKitCollection = Map Color FaceViewKit

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
        base = -0.3
        length = 0.6
    in fromLists [[base, (-hw), 0, 1], [base, hw, 0, 1], [base+length, 0, 0, 1]]

arrowPoints :: (Rotation,Int) -> Matrix Float
arrowPoints (rotation,index) = 
    let cwRotations = [3*pi/2, pi, pi/2, 0]
        cwTranslations = [(0.5,0.5,0)
                         ,(0.5,2.5,0)
                         ,(2.5,2.5,0)
                         ,(2.5,0.5,0)
                         ]

        cwTransformations = zipWith multStd2 
                              (fmap xyRotationMatrix cwRotations) 
                              (fmap translationMatrix cwTranslations)

        ccwRotations = [pi/2, 0, 3*pi/2, pi]
        ccwTranslations = [(0.5,1.5,0),(1.5,2.5,0),(2.5,1.5,0),(1.5,0.5,0)]

        ccwTransformations = zipWith multStd2 
                              (fmap xyRotationMatrix ccwRotations) 
                              (fmap translationMatrix ccwTranslations)

        transformations = if rotation == CW then cwTransformations else ccwTransformations
        transform = transformations !! index
    in arrow `multStd2` transform

showArrow :: MonadWidget t m => (Rotation, Int) -> Dynamic t FaceViewKit -> m (Event t Action)
showArrow arrowIndex@(rotation,_) dFaceViewKit = do
    let points = arrowPoints arrowIndex
    dFacet <- mapDyn face dFaceViewKit  
    dAttrs <- mapDyn (\fvk -> "fill" =: "grey" <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points)) dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return $ attachWith (\a _ -> RotateFace rotation a)  (current dFacet) $ domEvent Click el

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
    left <-       showAndAdvance 0 0 east lowerLeft   -- lower left
    upperLeft <-  showAndAdvance 0 1 east left        -- left
    upper <-      showAndAdvance 0 2 east upperLeft   -- upper left
    upperRight <- showAndAdvance 1 2 east upper       -- upper
    right <-      showAndAdvance 2 2 east upperRight  -- upper right
    lowerRight <- showAndAdvance 2 1 east right       -- right
    lower <-      showAndAdvance 2 0 east lowerRight  -- lower right
    center <-     showAndAdvance 1 0 south lower      -- lower
    _ <-          showFacet      1 1       center          

    leftFace <-      advance (south.north) left
    leftEv <-        showArrow (CCW,0) leftFace
    leftCornerEv <-  showArrow (CW,0) leftFace

    upperFace <-     advance (south.north) upper
    upperEv <-       showArrow (CCW,1) upperFace
    upperCornerEv <- showArrow (CW,1) upperFace

    rightFace <-     advance (south.north) right
    rightEv <-       showArrow (CCW,2) rightFace
    rightCornerEv <- showArrow (CW,2) rightFace

    lowerFace <-     advance (south.north) lower
    lowerEv <-       showArrow (CCW,3) lowerFace
    lowerCornerEv <- showArrow (CW,3) lowerFace

    return $ leftmost [ leftEv , leftCornerEv 
                      , upperEv , upperCornerEv 
                      , rightEv , rightCornerEv 
                      , lowerEv , lowerCornerEv 
                      ]

showInside :: MonadWidget t m => Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showInside dFaceViewKit = showFacetRectangle 0 0 3 3 =<< mapDyn (changeViewKitColor Maroon) dFaceViewKit 

showUpperMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showUpperMiddleFace upperRight = do  
    upper <-       showAndAdvance 2 2 south upperRight  
    upperLeft <-  showAndAdvance 1 2 west upper      
                    >>= showFacet 0 2                 

    upperFace <-     advance (south.north) upper
    upperCornerEv <- showArrow (CW,1) upperFace
    upperEv <-       showArrow (CCW,1) upperFace

    rightFace <-     advance (south.north.east) upperRight
    rightCornerEv <- showArrow (CW,2) rightFace

    return $ leftmost [ upperEv , upperCornerEv 
                                , rightCornerEv 
                      ]


showMiddleMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showMiddleMiddleFace upperRight = do  
    right <-      advance east upperRight 
    _     <-      showAndAdvance 2 1 south right 
              >>= showFacet 1 1            

    left <-       advance (south.west.south) upperRight
              >>= showFacet 0 1            

    rightFace <-     advance (south.north) right
    rightEv <-       showArrow (CCW,2) rightFace

    leftFace <-      advance (south.north) left
    leftEv <-        showArrow (CCW,0) leftFace


    return $ leftmost [ leftEv 
                      , rightEv 
                      ]

showLowerMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showLowerMiddleFace lowerLeft = do  
    lower <-       showAndAdvance 0 0 south lowerLeft  
    lowerRight <-  showAndAdvance 1 0 west lower      
                    >>= showFacet 2 0                 

    lowerFace <-     advance (south.north) lower
    lowerCornerEv <- showArrow (CW,3) lowerFace
    lowerEv <-       showArrow (CCW,3) lowerFace

    leftFace <-     advance (south.north.east) lowerLeft
    leftCornerEv <- showArrow (CW,0) leftFace

    return $ leftmost [ lowerEv , lowerCornerEv 
                                , leftCornerEv 
                      ]

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

viewTransformation :: Model -> Facet -> Bool -> Float -> (Matrix Float, Bool)
viewTransformation model@(Model topFace orientation twist twistMode) viewCenterFacet withTwist offset = 
    let faceColor = color viewCenterFacet 
        topColor = color topFace

        scale2dMatrix = scaleMatrix (1/3) -- scale from 3x3 square face to 1x1 square face.
        trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = translationMatrix (trans2d,trans2d,0)


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
            if withTwist && twist /= 0
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
    in (modelTransform, isFacingCamera)

insideFacesCamera :: Model -> Facet -> Bool
insideFacesCamera model facet = 
    let (_,isFacingCamera) = viewTransformation model facet False (1.0/6.0)
    in isFacingCamera

viewKit :: Model -> Facet -> Bool -> Float -> (Bool, FaceViewKit)
viewKit model@(Model topFace orientation twist twistMode) viewFacet withTwist offset = 
    let viewCenterFacet = (south.south) viewFacet 
        (modelTransform, isFacingCamera) 
            = viewTransformation model viewCenterFacet withTwist offset

        -- scale up to svg box scale
        viewScaleMatrix = scaleMatrix viewScale

        -- move to center of svg box
        viewTranslationMatrix = translationMatrix (viewScale/2, viewScale/2, 0)

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspectivePrepMatrix
                        `multStd2` perspectiveMatrix
                        `multStd2` viewScaleMatrix
                        `multStd2` viewTranslationMatrix

    in (isFacingCamera, FaceViewKit viewFacet viewTransform)

kitmapUpdate :: Model -> Bool -> Float -> ViewKitCollection -> Facet -> ViewKitCollection
kitmapUpdate model withTwist offset prevMap lowerLeft = 
    let (isVisible, newViewKit) 
            = viewKit model lowerLeft withTwist offset
    in  if isVisible 
        then insert ((color.south.south) lowerLeft) newViewKit prevMap
        else prevMap

topView :: Model -> ViewKitCollection
topView model@(Model center _ _ twistMode)  =
    foldl (kitmapUpdate model (twistMode == TopTwist) (1.0/2.0)) empty [getLowerLeft center]

middleUpView :: Model -> ViewKitCollection
middleUpView model@(Model center _ twist twistMode)  =
    if twist == 0 || twistMode /= TopTwist
    then empty
    else foldl (kitmapUpdate model False (1.0/6.0)) empty [getLowerLeft center]

bottomUpView :: Model -> ViewKitCollection
bottomUpView model@(Model center _ twist twistMode)  =
    if twist == 0 || twistMode /= BottomTwist
    then empty
    else foldl (kitmapUpdate model True (-1.0/6.0)) empty [getLowerLeft center]

upperRights :: Model -> [Facet]
upperRights model@(Model center _ _ _)   =
    let upperRight = (north.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
    in scanl (&) upperRight advancers  -- get upper left corners of all faces

upperMiddleView :: Model -> ViewKitCollection
upperMiddleView model@(Model center _ _ twistMode)   =
    foldl (kitmapUpdate model (twistMode == TopTwist) 0.5) empty $ upperRights model

middleMiddleView :: Model -> ViewKitCollection
middleMiddleView model@(Model center _ _ twistMode)   =
    foldl (kitmapUpdate model False 0.5) empty $ upperRights model


bottomView :: Model -> ViewKitCollection
bottomView model@(Model center _ _ twistMode)  =
    foldl (kitmapUpdate model (twistMode == BottomTwist) 0.5) empty [(west.south.west.west.south.west.getLowerLeft) center]

lowerMiddleView :: Model -> ViewKitCollection
lowerMiddleView model@(Model center _ _ twistMode)  =
    let lowerLeft = (west.south.west.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
        lowerLefts = scanl (&) lowerLeft advancers  -- get lower left corners of all faces
    in foldl (kitmapUpdate model (twistMode == BottomTwist) 0.5) empty lowerLefts

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
    bottomUpMap <-                  mapDyn bottomUpView model
    middleMiddleMap <-              mapDyn middleMiddleView model
    middleUpMap <-                  mapDyn middleUpView model
    upperMiddleMap <-               mapDyn upperMiddleView model
    topMap <-                       mapDyn topView model

    bottomEventsWithKeys <-         listWithKey bottomMap $ const showFace
    lowerMiddleEventsWithKeys <-    listWithKey lowerMiddleMap $ const showLowerMiddleFace
    _ <-                            listWithKey bottomUpMap $ const showInside
    middleMiddleEventsWithKeys <-   listWithKey middleMiddleMap $ const showMiddleMiddleFace
    _ <-                            listWithKey middleUpMap $ const showInside
    upperMiddleEventsWithKeys <-    listWithKey upperMiddleMap $ const showUpperMiddleFace
    topEventsWithKeys <-            listWithKey topMap $ const showFace

    let topEvent = switch $ (leftmost . elems) <$> current topEventsWithKeys
        bottomEvent = switch $ (leftmost . elems) <$> current bottomEventsWithKeys
        lowerMiddleEvent = switch $ (leftmost . elems) <$> current lowerMiddleEventsWithKeys
        middleMiddleEvent = switch $ (leftmost . elems) <$> current middleMiddleEventsWithKeys
        upperMiddleEvent = switch $ (leftmost . elems) <$> current upperMiddleEventsWithKeys
    return $ leftmost [topEvent, lowerMiddleEvent, middleMiddleEvent, upperMiddleEvent, bottomEvent]

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
                       (constDyn $  "width" =: show viewScale
                                 <> "height" =: show viewScale
                                 ) $ viewModel model
        return $ leftmost [ev, leftEv, rightEv, upEv, downEv]

