import Ros.Node
import Ros.Topic.Util (topicRate, concats)
import Data.Default.Generics (def)
import Lens.Family ((.~), (&))

import Improv
import RobotSpec
import Parser
import qualified Control.Exception as E
import System.Environment

-- robotRate, moveTopic should be defined in RobotSpec module

main = do args <- getArgs
          case args of
              [] -> do doc <- readFile "./src/test.imp"
                       case parseFile doc of 
                            Right moveTopic -> runNode "HaskellTurtle" $ advertise "turtle1/cmd_vel" $ topicRate 1 moveTopic
                            Left (ParseErr line err) -> putStrLn $ "Parse error on line " ++ (show line) ++ ": " ++ err
              _  -> putStrLn "Incorrect number of arguments: need file name"
