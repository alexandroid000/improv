import Ros.Node
import Ros.Topic.Util (topicRate, concats)
import Data.Default.Generics (def)
import Lens.Family ((.~), (&))

import Improv
import RobotSpec
import Demo

-- robotRate, moveTopic should be defined in RobotSpec module

main =  runNode "HaskellTurtle" $
        advertise "turtle1/cmd_vel" $
        (topicRate (fromIntegral robotRate) moveTopic) -- publish once per second
        
