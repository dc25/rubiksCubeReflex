{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Data.Map (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Control.Monad.Reader

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show,Eq,Ord,Enum)

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
getRotationMap :: Facet -> RotationMap
getRotationMap f =
    let 
        ff :: (Facet, Facet, RotationMap) -> (Facet -> Facet, Facet -> Facet) -> (Facet, Facet, RotationMap)
        ff (pre, post, oldRotMap) (splitDown, advanceDirection) =
            let rm' =  insert (pre, splitDown pre) (splitDown post) oldRotMap
                newRotMap =  insert (splitDown post, post) pre rm'
            in (advanceDirection pre, advanceDirection post, newRotMap)

        -- faceCrawl = [(east, south), (south, west), (south, west)]
        faceCrawl = [(east, north), (south, east), (south, east)]
        fullCrawl = concat $ replicate 4 faceCrawl
        preStart = east.north.north $ f
        postStart = foldl (&) preStart (map snd faceCrawl)
        (_,_,rotationMap) = foldl ff (preStart, postStart, empty) fullCrawl

    in rotationMap

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

    return [switch $ (leftmost . concat . elems) <$> current eventsWithKeys]

showFacetMarkerHole :: MonadWidget t m => Int -> Int -> Float -> Dynamic t Facet -> Dynamic t [FacetSig] -> ReaderT (Dynamic t Model) m [Event t ()]
showFacetMarkerHole row col margin dFacet dShowList = do
    dSelectableFacet <- mapDyn (\(sSigs, fa) -> const (0 :: Int, show $ val fa) <$> filter (\s -> signature fa == s) sSigs) =<< combineDyn (,) dShowList dFacet
    moveMap <- mapDyn fromList dSelectableFacet
    eventsWithKeys <- listWithKey moveMap (\_ color -> showFacetSquare row col margin color)

    return [switch $ (leftmost . concat . elems) <$> current eventsWithKeys]


showFacet :: MonadWidget t m => Int -> Int -> Dynamic t Facet -> ReaderT (Dynamic t Model) m (Event t Action)
showFacet row col dFacet = do
    dModel <- ask
    dSignature <- mapDyn signature dFacet
    dFacetColor <- mapDyn (show.val) dFacet

    dSelectableSigs <- mapDyn selectables dModel
    dReferenceSigs <- mapDyn (fmap signature.maybeToList.reference) dModel

    outlineClick <- showFacetSquare row col 0.0 $ constDyn "black"
    elClick <- showFacetSquare row col 0.05 dFacetColor
    promptClick <- showFacetMarker row col 0.3 dFacet dSelectableSigs
    pc2 <- showFacetMarkerHole row col 0.4 dFacet dSelectableSigs

    referenceClick <- showFacetMarker row col 0.3 dFacet dReferenceSigs

    let facetClick = leftmost $ elClick ++ outlineClick ++ promptClick ++ pc2 ++ referenceClick

    return $ attachWith (\a _ -> FacetSelect a)  (current dFacet) facetClick

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
view model = showCube =<< mapDyn cube model

data Action = FacetSelect Facet

data Model = Model { cube :: Facet 
                   , selectables :: [FacetSig]
                   , reference :: Maybe Facet
                   }

cornerSignatures = [(color, index) | color <- [ Red .. Purple ] , index <- [1,3,5,7] ] 

targets :: Facet -> [FacetSig]
targets f = fmap signature [ (west.south) f, (east.east) f ]

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
        case action of
            FacetSelect facet -> 
                let facetSig = signature facet 
                in case reference model of
                   Nothing ->  Model (rotateFace $ cube model)
                              (if facetSig `elem` selectables model then targets facet else selectables model)
                              (Just facet)
                   Just ref ->  Model (rotateFace $ cube model) 
                              (if facetSig `elem` selectables model then [] else selectables model)
                              Nothing

initModel = Model mkCube cornerSignatures Nothing

main = mainWidget $ do 
           rec
               selectEvent <- runReaderT (view model) model
               model <- foldDyn update initModel selectEvent
           return ()
