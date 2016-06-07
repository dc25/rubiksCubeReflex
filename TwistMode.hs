module TwistMode (TwistMode(BottomTwist, TopTwist, NoTwist)) where

data TwistMode = BottomTwist | TopTwist | NoTwist deriving (Show,Eq,Ord,Enum)
