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


-- resolution: can change action at most robotRate times per second
-- Actions will be stretched to fill 1 second
robotRate = 100


core :: KineChain Double
core = Link (O 0) 0

type Translation = Double
type Rotation = Double

-- define DOF
-- for DDR: forward translation, yaw rotation
data VelCmd a = VelCmd a a

-- average directions, allowing finer motor control
composeVels :: (Fractional a) => VelCmd a -> VelCmd a -> VelCmd a
composeVels (VelCmd t1 r1) (VelCmd t2 r2) = VelCmd ((t1+t2)/2) ((r1+r2)/2)

instance Functor VelCmd where
    fmap f (VelCmd t r) = VelCmd (f t) (f r)

move :: Robot Double -> Action -> [VelCmd Double]
-- Primitives
move r (A o Center _) = [VelCmd 0 0] -- no articulation
move r (A o _ Zero) = [VelCmd 0 0] -- no articulation
move r (A o Lef Quarter) = [VelCmd 0 (pi/2)] -- rad/sec
move r (A o Forward Quarter) = [VelCmd 1 0] -- meters/sec
move r (As []) = []
move r (As acts) = concatMap (move r) acts

-- Derived from primitives
move r (A o Righ d) = map (fmap negate) (move r (A o Lef d))
move r (A o Backward d) = map (fmap negate) (move r (A o Forward d))
move r (A o d Half) = map (fmap (*2)) (move r (A o d Quarter))
move r (A o d ThreeFourths) = map (fmap (*3)) (move r (A o d Quarter))
move r (A o d Full) = map (fmap (*4)) (move r (A o d Quarter))
move r (A o (d1 :*: d2) len) =
    let r1 = move r (A o d1 len)
        r2 = move r (A o d2 len)
    in  [composeVels (head r1) (head r2)]

mkTwist :: VelCmd Double -> Twist
mkTwist (VelCmd t r) = def  & angular . V.z .~ r
                            & linear . V.x .~ t

-- input in radians, ccw
rotate :: Double -> [VelCmd Double]
rotate ang =
    let a = (signum ang)*(mod' ang (2*pi))
    in  [VelCmd 0 a]

translate :: Double -> [VelCmd Double]
translate d = [VelCmd d 0]


moveCommands :: [VelCmd Double] -> Topic IO Twist
moveCommands cfs = concats $ repeatM $ twisties
    where twisties = return $ (map mkTwist cfs) ++ (repeat $ mkTwist (VelCmd 0 0))
