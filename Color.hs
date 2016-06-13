module Color ( Color( Red 
                    , Green 
                    , Blue 
                    , Yellow 
                    , Orange 
                    , Purple 
                    , Black 
                    , Maroon  )
             ) where

data Color = Red | Green | Blue | Yellow | Orange | Purple | Black | Maroon deriving (Show,Eq,Ord,Enum)
