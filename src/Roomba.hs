module Roomba where

import Improv
-- roomba poses
-- l: limb
-- o: origin
-- d: direction
-- s: size

-- roomba only has core
-- to move in a direction, the roomba must first turn and face that direction
-- size of 0 means just face that direction, don't move
-- each incremented size beyond that means move in that direction 

roomba :: KineChain Double
roomba = Root 0

type Translation = Double
type Rotation = Double

-- define DOF
-- for DDR: forward translation, yaw rotation
data Config a = Config a a

-- average directions, allowing finer motor control
composeDirs :: (Fractional a) => Config a -> Config a -> Config a
composeDirs (Config t1 r1) (Config t2 r2) = Config ((t1+t2)/2) ((r1+r2)/2)

instance Functor Config where
    fmap f (Config t r) = Config (f t) (f r)

moveToPose :: Robot Double -> Action -> Config Double
-- Primitives
moveToPose r (A o Center _) = Config 0 0 -- no articulation
moveToPose r (A Origin Lef Zero) = Config 0 (pi/2) -- rad/sec
moveToPose r (A Origin _ Quarter) = Config 1 0 -- meters/sec

-- Derived from primitives
moveToPose r (A o (d1 :*: d2) len) =
    let r1 = moveToPose r (A o d1 len)
        r2 = moveToPose r (A o d2 len)
    in  composeDirs r1 r2
moveToPose r (A o Righ d) = fmap negate (moveToPose r (A o Lef d))
moveToPose r (A o Backward d) = fmap negate (moveToPose r (A o Forward d))
moveToPose r (A o d Half) = fmap (*2) (moveToPose r (A o d Quarter))
moveToPose r (A o d ThreeFourths) = fmap (*3) (moveToPose r (A o d Quarter))
moveToPose r (A o d Full) = fmap (*4) (moveToPose r (A o d Quarter))

