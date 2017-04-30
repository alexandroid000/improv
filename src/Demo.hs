module Demo where

import RobotSpec
import Improv

left :: Action
left = A o Lef Zero
       where o = origin core
right = A o Righ Zero
    where o = origin core
forward = A o Forward Quarter
    where o = origin core

tree :: Double -> Int -> [VelCmd Double]
tree len depth
    | depth == 0 = (translate len)
    | otherwise = concat
                  [tree (len/3) (depth-1)
                  , rotate (pi/3)
                  , tree (len/3) (depth-1)
                  , rotate (-2*pi/3)
                  , tree (len/3) (depth-1)
                  , rotate (pi/3)
                  , tree (len/3) (depth-1)
                  ]


--zig = [map (effort 3) left*3, right]
--zag = map reverse zig
--zigzag = [zig, zag] ++ zigzag
outAndBack = As [forward, right, right, forward]
--moveTopic :: Topic IO Twist
--moveTopic = moveCommands $ concat $ repeat $ concat  [
--            move core outAndBack,
--            rotate (pi/3)
--            --, move core [left*3]
--            --, move core (take 10 $ mconcat zigzag)
--            ]

moveTopic = moveCommands (tree 100 4)
