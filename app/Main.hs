import Improv
import Ros.Geometry_msgs.Twist
import qualified Ros.Std_msgs.String as S
import qualified Ros.Geometry_msgs.Vector3 as V
import Ros.Node
import Ros.Topic (cons, repeatM)
import Ros.Topic.Util (topicRate)
import Data.Default.Generics (def)
import Lens.Family ((.~), (&))
import Data.Time.Clock (getCurrentTime)

-- roomba poses
-- l: limb
-- o: origin
-- d: direction
-- s: size

-- roomba only has core
-- to move in a direction, the roomba must first turn and face that direction
-- size of 0 means just face that direction, don't move
-- each incremented size beyond that means move in that direction 

data Limb = C1


type Size = Int


moveToPose :: Limb -> Origin -> Direction -> Size -> (Double, Double)
moveToPose C1 Origin Lef s = (0, size*pi/4)
    where size = fromIntegral s


-- hacky extra parameter so we can map over getCurrentTime
mkTwist :: (Double, Double) -> a -> Twist
mkTwist (l,a) t = def & linear . V.x .~ l
                      & angular . V.z .~ a


sayHello :: Topic IO S.String
sayHello = repeatM (fmap mkMsg getCurrentTime)
    where mkMsg = S.String . ("Hello world " ++) . show

moveTopic :: Topic IO Twist
moveTopic = repeatM $ fmap (mkTwist (moveToPose C1 Origin Lef 2)) getCurrentTime

main =  runNode "HaskellTurtle" $
        advertise "chatter" $ (topicRate 1 sayHello)
        
