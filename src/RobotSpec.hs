module RobotSpec where

import Improv
import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import Ros.Node
import Ros.Topic (cons, repeatM)
import Ros.Topic.Util (topicRate, concats)
import Data.Default.Generics (def)
import Lens.Family ((.~), (&))
import Data.Fixed
-- roomba poses
-- l: limb
-- o: origin
-- d: direction
-- s: size

-- roomba only has core
-- to move in a direction, the roomba must first turn and face that direction
-- size of 0 means just face that direction, don't move
-- each incremented size beyond that means move in that direction


-- resolution: can change action at most robotRes times per second
robotRes = 100 :: Mult -- messages/second, ROS publishing rate
robotRate = 1 :: Mult -- seconds per primitive

core :: KineChain Double
core = Link (O 0) 0

type Translation = Double
type Rotation = Double

-- define DOF
-- for DDR: forward translation, yaw rotation
data VelCmd a = VelCmd a a
    deriving (Eq, Show)

instance Functor VelCmd where
    fmap f (VelCmd t r) = VelCmd (f t) (f r)

-- average directions, allowing finer motor control
averageVels :: (Fractional a) => VelCmd a -> VelCmd a -> VelCmd a
averageVels (VelCmd t1 r1) (VelCmd t2 r2) = VelCmd ((t1+t2)/2) ((r1+r2)/2)

addVels :: (Fractional a) => VelCmd a -> VelCmd a -> VelCmd a
addVels (VelCmd t1 r1) (VelCmd t2 r2) = VelCmd (t1+t2) (r1+r2)

danceToMsg :: (Parts a) => Dance a -> [VelCmd Double]
danceToMsg (Prim a m _) = map (increaseVel m) $
                          take (round $ robotRes*robotRate/m) $
                          repeat (moveBase a)
danceToMsg Skip         = []
danceToMsg (Rest m)     = take (round $ robotRes*robotRate/m) $ repeat (VelCmd 0 0)
danceToMsg (d1 :||: Skip) = danceToMsg d1
danceToMsg (d1 :||: d2) = zipWith (addVels) (danceToMsg d1) (danceToMsg d2)
danceToMsg (d1 :+: d2)  = (danceToMsg d1) ++ (danceToMsg d2)


moveBase :: Action -> VelCmd Double
-- Primitives
moveBase (A Center _)           = VelCmd 0 0 -- no articulation
moveBase (A _ Zero)             = VelCmd 0 0 -- no articulation
moveBase (A Lef Eighth)        = VelCmd 0 (pi/4) -- rad/sec
moveBase (A Lef Quarter)        = VelCmd 0 (pi/2) -- rad/sec
moveBase (A Forward Quarter)    = VelCmd 0.25 0 -- meters/sec

-- Derived from primitives
moveBase (A Righ d)             = (fmap negate) (moveBase (A Lef d))
moveBase (A Backward d)         = (fmap negate) (moveBase (A Forward d))
moveBase (A d Half)             = (fmap (*2)) (moveBase (A d Quarter))
moveBase (A d ThreeFourths)     = (fmap (*3)) (moveBase (A d Quarter))
moveBase (A d Full)             = (fmap (*4)) (moveBase (A d Quarter))
moveBase (A (d1 :*: d2) len) =
    let r1 = moveBase (A d1 len)
        r2 = moveBase (A d2 len)
    in  averageVels r1 r2

increaseVel :: Mult -> VelCmd Double -> VelCmd Double
increaseVel mult vcmd = fmap (*mult) vcmd

mkTwist :: VelCmd Double -> Twist
mkTwist (VelCmd t r) = def  & angular . V.z .~ r
                            & linear . V.x .~ t

-- input in radians, ccw
rotate :: Double -> VelCmd Double
rotate ang =
    let a = (signum ang)*(mod' ang (2*pi))
    in  VelCmd 0 a

translate :: Double -> VelCmd Double
translate d = VelCmd d 0


moveCommands :: [VelCmd Double] -> Topic IO Twist
moveCommands cfs = concats $ repeatM $ twisties
    where twisties = return $ (map mkTwist cfs) ++ (repeat $ mkTwist (VelCmd 0 0))
