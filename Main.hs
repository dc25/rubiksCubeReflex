import Reflex.Dom

data Color = Red 
           | Green 
           | Blue 
           | Yellow 
           | Orange 
           | Purple 
           deriving Show

data DNode a = DNode { north :: DNode a
                     , west :: DNode a
                     , south  :: DNode a
                     , east  :: DNode a
                     , val   :: a
                     }

type Facet = DNode Color

type Edge = (Facet,Facet,Facet)

mkDomino :: Facet -> Facet -> Facet -> Facet -> Facet -> Facet -> Color -> (Facet,Facet)
mkDomino right upperRight upperLeft left lowerLeft lowerRight color =
    let dominoRight = DNode upperRight dominoLeft lowerRight right       color
        dominoLeft =  DNode upperLeft  left       lowerLeft  dominoRight color
    in (dominoLeft, dominoRight)

mkFace :: Edge -> Edge -> Edge -> Edge -> Color -> (Edge,Edge,Edge,Edge)
mkFace ~(nRight, nCenter, nLeft) 
       ~(wRight, wCenter, wLeft) 
       ~(sRight, sCenter, sLeft) 
       ~(eRight, eCenter, eLeft) 
       color =

    let center = DNode nSide wSide sSide eSide color
        (nwCorner, nSide) = mkDomino enCorner nCenter nLeft wRight wSide center color
        (wsCorner, wSide) = mkDomino nwCorner wCenter wLeft sRight sSide center color
        (seCorner, sSide) = mkDomino wsCorner sCenter sLeft eRight eSide center color
        (enCorner, eSide) = mkDomino seCorner eCenter eLeft nRight nSide center color

    in ( (nwCorner, nSide, enCorner)
       , (wsCorner, wSide, nwCorner)
       , (seCorner, sSide, wsCorner)
       , (enCorner, eSide, seCorner)
       )


main = 
          let  (nPurple, wPurple, sPurple,  ePurple) =   mkFace nGreen    nBlue     nYellow   nRed       Purple

               (nYellow, wYellow, sYellow,  eYellow) =   mkFace sPurple   eBlue     nOrange   wRed       Yellow
               (nBlue,   wBlue,   sBlue,    eBlue)   =   mkFace wPurple   eGreen    wOrange   wYellow    Blue
               (nGreen,  wGreen,  sGreen,   eGreen)  =   mkFace nPurple   eRed      sOrange   wBlue      Green
               (nRed,    wRed,    sRed,     eRed)    =   mkFace ePurple   eYellow   eOrange   wGreen     Red

               (nOrange, wOrange, sOrange, eOrange)  =   mkFace sYellow   sBlue     sGreen    sRed       Orange
               (purpleCorner,_,_) = nPurple
               purpleCenter = south $ south purpleCorner
               greenCenter = south $ north $ north purpleCenter
               blueCenter = south $ north $ east greenCenter
               redCenter = south $ north $ east purpleCenter
               orangeCenter = south $ north $ south greenCenter
               yellowCenter = south $ north $ east blueCenter
          in mainWidget $ do 
                             el "div" $ showFace purpleCenter
                             el "div" $ showFace greenCenter
                             el "div" $ showFace blueCenter
                             el "div" $ showFace redCenter
                             el "div" $ showFace orangeCenter
                             el "div" $ showFace yellowCenter

showFace f = do 
                text $ show $ val f 

                let nf = north f
                text $ show $ val nf 
                text $ show $ val $ west nf 

                let sf = south f
                text $ show $ val sf 
                text $ show $ val $ west sf 

                let ef = east f
                text $ show $ val ef 
                text $ show $ val $ west ef 

                let wf = west f
                text $ show $ val wf 
                text $ show $ val $ west wf 


