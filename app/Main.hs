import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import Ros.Node
import Ros.Topic (cons, repeatM)
import Ros.Topic.Util (topicRate, concats)
import Data.Default.Generics (def)
import Lens.Family ((.~), (&))
import Data.Time.Clock (getCurrentTime)

import Improv
import Roomba

-- hacky extra parameter so we can map over getCurrentTime
mkTwist :: Config Double -> Twist
mkTwist (Config r t) = def & linear . V.x .~ t
                           & angular . V.z .~ r


-- compile list of movements to a list of "moveToPose" functions
-- zip with getCurrentTime
-- publish at rate of 1Hz
moveTopic :: Topic IO Twist
moveTopic = moveCommands [turnLeft, turnLeft, turnLeft]
    where turnLeft = moveToPose roomba (A Origin Lef Quarter)

moveCommands :: [Config Double] -> Topic IO Twist
moveCommands cfs = concats $ repeatM $ twisties
    where twisties = return $ (map mkTwist cfs) ++ (repeat $ mkTwist (Config 0 0))

main =  runNode "HaskellTurtle" $
        advertise "chatter" $ (topicRate 1 moveTopic) -- publish once per second
        
