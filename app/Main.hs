{-# LANGUAGE OverloadedStrings #-}

import Ros.Node
import Ros.Topic.Util (topicRate)
import Ros.Geometry_msgs.Twist as Twist
--import Data.Default.Generics (def)
--import Lens.Family ((.~), (&))

import Parser
import RobotSpec
--import qualified Control.Exception as E
import System.Environment
import qualified Data.Map as Map
import Ros.Turtlesim.Pose as P
import Lens.Family (view)

-- robotRate, moveTopic should be defined in RobotSpec module

showMsg :: P.Pose -> IO ((Float,Float))
showMsg pose = return (P._x pose, P._y pose)

get_pose :: Node ((Topic IO) Twist)
get_pose = subscribe "/turtle1/pose"

main :: IO ()
main = do doc <- readFile "./src/test.imp"
          case convertFile doc of 
               Right moveTopics -> runNode "HaskellTurtle" $
                     do mapM (\(name, twist) -> advertise (name ++ "/cmd_vel") (topicRate robotRes twist)) $ Map.toList moveTopics
               Left (ParseErr (-1) err) -> putStrLn $ "Parse error " ++ err
               Left (ParseErr line err) -> putStrLn $ "Error (line " ++ (show line) ++ "):\n" ++ err