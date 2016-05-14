import Reflex.Dom

data Color = Red | Green | Blue | Yellow | Orange | White

data DWeb a = DWeb { north :: DWeb a
                   , south :: DWeb a
                   , west  :: DWeb a
                   , east  :: DWeb a
                   , value :: a
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
mkWeb0 (n:ns) (s:ss) es ws rowCount colCount v = 
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
        (rest, last) = mkChain0 this s es ws (rowCount-1) v
    in (this, last)


main = mainWidget $ 
           let d = 3
               (b0n, b0s, b0w, b0e) = mkWeb b3e b1e b2e b4e d d Yellow

               (b1n, b1s, b1w, b1e) = mkWeb b2s b4n b5n b0s d d Red
               (b2n, b2s, b2w, b2e) = mkWeb b3s b1n b5w b0w d d Green
               (b3n, b3s, b3w, b3e) = mkWeb b4s b2n b5s b0n d d Blue
               (b4n, b4s, b4w, b4e) = mkWeb b1s b3n b5e b0e d d Yellow

               (b5n, b5s, b5w, b5e) = mkWeb b1w b3w b4w b2w d d Yellow
           in text "hello world"
