import Reflex.Dom

data Color = Red | Green | Blue | Yellow | Orange | White deriving Show

data DWeb a = DWeb { north :: DWeb a
                   , south :: DWeb a
                   , west  :: DWeb a
                   , east  :: DWeb a
                   , val   :: a
              }

mkList :: DWeb a -> (DWeb a -> DWeb a) -> Int -> [DWeb a]
mkList _ _ 0 = []
mkList dw advance c = dw:mkList (advance dw) advance (c-1)

mkWeb :: [DWeb a] -> [DWeb a] -> [DWeb a] -> [DWeb a] -> Int -> Int -> a -> ([DWeb a], [DWeb a], [DWeb a], [DWeb a])
mkWeb ns ss ws es rowCount colCount v = 
    let (w, e) = mkWeb0 ns ss ws es rowCount colCount v
    in (mkList (head w) east colCount, mkList (head e) west colCount, w, e)

mkWeb0 :: [DWeb a] -> [DWeb a] -> [DWeb a] -> [DWeb a] -> Int -> Int -> a -> ([DWeb a], [DWeb a])
mkWeb0 _ _ ws es _ 0 _ = (es, ws)
mkWeb0 (n:ns) (s:ss) ws es rowCount colCount v = 
    let this = mkChain n s ws rest rowCount v
        (rest, last) = mkWeb0 ns ss this es rowCount (colCount-1) v
    in (this, last)

mkChain :: DWeb a -> DWeb a -> [DWeb a] -> [DWeb a] -> Int -> a -> [DWeb a]
mkChain n s ws es rowCount v = 
    let (f,_) = mkChain0 n s ws es rowCount v
    in mkList f south rowCount

mkChain0 :: DWeb a -> DWeb a -> [DWeb a] -> [DWeb a] -> Int -> a -> (DWeb a, DWeb a)
mkChain0 n s _ _ 0 v = (s,n)
mkChain0 n s (w:ws) (e:es) rowCount v = 
    let this = DWeb n rest w e v 
        (rest, last) = mkChain0 this s ws es (rowCount-1) v
    in (this, last)

data Model = Model { cube :: DWeb Color }

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


main = mainWidget $ do
           let d = 3
               (b0n, b0s, b0w, b0e) = mkWeb (reverse b3e) b1e b2e (reverse b4e) d d Yellow

               (b1n, b1s, b1w, b1e) = mkWeb b2s b4n b5n b0s d d Red
               (b2n, b2s, b2w, b2e) = mkWeb b3s b1n (reverse b5w) b0w d d Green
               (b3n, b3s, b3w, b3e) = mkWeb b4s b2n (reverse b5s) (reverse b0n) d d Blue
               (b4n, b4s, b4w, b4e) = mkWeb b1s b3n b5e (reverse b0e) d d Orange

               (b5n, b5s, b5w, b5e) = mkWeb b1w (reverse b3w) (reverse b2w) b4w d d White

               yellowCorner = head b0n
               yellowCenter = south $ east yellowCorner 

               redCorner = head b1n
               redCenter = south $ east redCorner 


           el "div" $ showFace yellowCenter
           el "div" $ showFace redCenter
