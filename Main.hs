{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl)
import Data.Monoid ((<>))
import Control.Monad.Reader

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show,Eq,Ord)

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

mkDomino :: Facet -> Facet -> Facet -> Facet -> Facet -> Facet -> Color -> Int -> (Facet,Facet)
mkDomino right upperRight upperLeft left lowerLeft lowerRight color index =
    let leftDomino =  DNode upperLeft  left       lowerLeft  rightDomino color index
        rightDomino = DNode upperRight leftDomino lowerRight right       color (index+1)
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
            let rm' =  insert (pre, splitDown pre) (splitDown post) oldRotMap
                newRotMap =  insert (splitDown post, post) pre rm'
            in (advanceDirection pre, advanceDirection post, newRotMap)

        crawlDirections = concat $ replicate 4 [(east, south), (south, west), (south, west)]
        (_,_,rm) = foldl ff ((east.north.north) f, (west.west.south.east.north.north) f, empty) crawlDirections

    in rm

rotateFace :: Facet -> Facet
rotateFace f = copyWithRotation (getRotationMap f) f

width = 230
height = 230

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t String -> ReaderT (Dynamic t Model) m [Event t ()]
showFacetSquare row col margin dColor = do
    attrs <- mapDyn (\color ->    "x" =: show ((fromIntegral col :: Float) + margin)
                               <> "y" =: show ((fromIntegral row :: Float) + margin)
                               <> "width" =: show (1.0 - 2.0 * margin)
                               <> "height" =: show (1.0 - 2.0 * margin)
                               <> "fill" =: color) dColor

    (el, _) <- elDynAttrNS' svgNamespace "rect" attrs $ return ()
    return [domEvent Click el]

showFacetMarker :: MonadWidget t m => Int -> Int -> Float -> Dynamic t Facet -> Dynamic t [FacetSig] -> ReaderT (Dynamic t Model) m [Event t ()]
showFacetMarker row col margin dFacet dShowList = do
    dSelectableFacet <- mapDyn (\(sSigs, fa) -> const (0 :: Int, "grey") <$> filter (\s -> signature fa == s) sSigs) =<< combineDyn (,) dShowList dFacet
    moveMap <- mapDyn fromList dSelectableFacet
    eventsWithKeys <- listWithKey moveMap (\_ color -> showFacetSquare row col margin color)

    return [switch $ fmap (leftmost . concat . elems) $ current eventsWithKeys]

showFacetMarkerHole :: MonadWidget t m => Int -> Int -> Float -> Dynamic t Facet -> Dynamic t [FacetSig] -> ReaderT (Dynamic t Model) m [Event t ()]
showFacetMarkerHole row col margin dFacet dShowList = do
    dSelectableFacet <- mapDyn (\(sSigs, fa) -> const (0 :: Int, show $ val fa) <$> filter (\s -> signature fa == s) sSigs) =<< combineDyn (,) dShowList dFacet
    moveMap <- mapDyn fromList dSelectableFacet
    eventsWithKeys <- listWithKey moveMap (\_ color -> showFacetSquare row col margin color)

    return [switch $ fmap (leftmost . concat . elems) $ current eventsWithKeys]


showFacet :: MonadWidget t m => Int -> Int -> Dynamic t Facet -> ReaderT (Dynamic t Model) m (Event t Action)
showFacet row col dFacet = do
    dModel <- ask
    dSignature <- mapDyn signature dFacet
    dFacetColor <- mapDyn (show.val) dFacet

    dSelectableSigs <- mapDyn selectables dModel


    outlineClick <- showFacetSquare row col 0.0 $ constDyn "black"
    elClick <- showFacetSquare row col 0.05 dFacetColor
    promptClick <- showFacetMarker row col 0.3 dFacet dSelectableSigs
    pc2 <- showFacetMarkerHole row col 0.4 dFacet dSelectableSigs


    let facetClick = leftmost $ elClick ++ outlineClick ++ promptClick ++ pc2

    return $ attachWith (\a _ -> FacetSelect a)  (current dSignature) facetClick

showFace :: MonadWidget t m => Dynamic t Facet -> ReaderT (Dynamic t Model) m (Event t Action)
showFace upperLeft = do
    (_, ev) <- elDynAttrNS' svgNamespace "svg" 
                   (constDyn $  "viewBox" =: "0 0 3 3 "
                             <> "width" =: show width
                             <> "height" =: show height)
                   $ do ulClick <- showFacet 0 0 upperLeft 
                        eOulClick <- showFacet 0 1 =<< mapDyn east upperLeft

                        upperRight <- mapDyn (east . east) upperLeft
                        urClick <- showFacet 0 2 upperRight 
                        eOurClick <- showFacet 1 2 =<< mapDyn east upperRight

                        lowerRight <- mapDyn (east . east) upperRight
                        lrClick <- showFacet 2 2 lowerRight 
                        eOlrClick <- showFacet 2 1 =<< mapDyn east lowerRight

                        lowerLeft <- mapDyn (east . east) lowerRight
                        llClick <- showFacet 2 0 lowerLeft 
                        eOllClick <- showFacet 1 0 =<< mapDyn east lowerLeft

                        center <- mapDyn (south . east) upperLeft
                        centerClick <- showFacet 1 1 center 

                        return $ leftmost [ ulClick 
                                          , eOulClick 
                                          , urClick 
                                          , eOurClick 
                                          , lrClick 
                                          , eOlrClick 
                                          , llClick 
                                          , eOllClick 
                                          , centerClick ]
    return ev

floatLeft = "style" =: "float:left" 
clearLeft = "style" =: "clear:left" 

showCube :: MonadWidget t m => Dynamic t Facet -> ReaderT (Dynamic t Model) m (Event t Action)
showCube cube = do
    purpleFace <- mapDyn (west.north) cube
    purpleClick <- el "div" $ showFace  purpleFace

    yellowFace <- mapDyn (west . west . south) purpleFace
    yellowClick <- elAttr "div" floatLeft $ showFace yellowFace

    redFace <- mapDyn (north . east . east) yellowFace
    redClick <- elAttr "div" floatLeft $ showFace redFace

    greenFace <- mapDyn (north . east . east) redFace
    greenClick <- elAttr "div" floatLeft $ showFace greenFace

    blueFace <- mapDyn (north . east . east) greenFace
    blueClick <- elAttr "div" floatLeft $ showFace blueFace 

    orangeFace <- mapDyn (west . west . south) yellowFace
    orangeClick <- elAttr "div" clearLeft $ showFace orangeFace

    return $ leftmost [ purpleClick
                      , yellowClick
                      , redClick
                      , greenClick
                      , blueClick
                      , orangeClick ]

view :: MonadWidget t m => Dynamic t Model -> ReaderT (Dynamic t Model) m (Event t Action)
view model = do 
    purpleFace <- mapDyn cube model
    pRot <- mapDyn (rotateFace.rotateFace) purpleFace
    showCube pRot

data Action = FacetSelect FacetSig

data Model = Model { cube :: Facet 
                   , selectables :: [FacetSig]
                   }

-- | FRP style update function.
-- | Given a board, an action and existing tour, return an updated tour.
update :: Action -> Model -> Model
update action model = 
        case action of
            FacetSelect facetSig -> Model (rotateFace $ cube model) [facetSig]

initModel = Model mkCube [(color, index) | color <- [ Red , Green , Blue , Yellow , Orange , Purple ] ,
                                           index <- [1,3,5,7] ]

main = mainWidget $ do 
           rec
               selectEvent <- runReaderT (view model) model
               model <- foldDyn update initModel selectEvent
           return ()
