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
mkFace (nRight, nCenter, nLeft) 
       (wRight, wCenter, wLeft) 
       (sRight, sCenter, sLeft) 
       (eRight, eCenter, eLeft) 
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


data Model = Model { cube :: Facet }

showFace f = do 
                let nf = north f
                text $ show $ val $ west nf 
                text $ show $ val nf 
                text $ show $ val $ east nf 

                text $ show $ val $ west f 
                text $ show $ val f 
                text $ show $ val $ east f 

                let sf = south f
                text $ show $ val $ west sf 
                text $ show $ val sf 
                text $ show $ val $ east sf 

main = 
          let  (nPurple, wPurple, sPurple,  ePurple) =   mkFace nGreen    nBlue     nYellow   nRed       Purple

               (nYellow, wYellow, sYellow,  eYellow) =   mkFace sPurple   eBlue     nOrange   wRed       Yellow
               (nBlue,   wBlue,   sBlue,    eBlue)   =   mkFace wPurple   eGreen    wOrange   wYellow    Blue
               (nGreen,  wGreen,  sGreen,   eGreen)  =   mkFace nPurple   eRed      sOrange   wBlue      Green
               (nRed,    wRed,    sRed,     eRed)    =   mkFace ePurple   eYellow   eOrange   wGreen     Red

               (nOrange, wOrange, sOrange, eOrange)  =   mkFace sYellow   sBlue     sGreen    sRed       Orange
          in mainWidget $ text "hello"
