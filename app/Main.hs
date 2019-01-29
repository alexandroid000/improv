{-# LANGUAGE OverloadedStrings #-}

import Ros.Node
import Ros.Topic.Util (topicRate)
import Ros.Geometry_msgs.Twist as Twist

import Parser
import RobotSpec
import System.Environment
import qualified Data.Map as Map
import Ros.Turtlesim.Pose as P
import Lens.Family (view)


data Engine = TurtleSim | Gazebo

engine = Gazebo
fname = "./user_instructions.txt"

main :: IO ()
main = do
    doc <- readFile fname
    case convertFile doc of
        Right moveTopics -> runNode "HaskellTurtle" $ do
            case engine of
                TurtleSim -> mapM (\(name, twist) -> advertise (name ++ "/cmd_vel") (topicRate robotRes twist)) $ Map.toList moveTopics
                Gazebo -> mapM (\(name, twist) -> advertise "/cmd_vel_mux/input/teleop" (topicRate robotRes twist)) $ Map.toList moveTopics
        Left (ParseErr (-1) err) -> putStrLn $ "Parse error " ++ err
        Left (ParseErr line err) -> putStrLn $ "Error (line " ++ (show line) ++ "):\n" ++ err



-- not being used right now, keeping as example of how to access pose
get_pose :: Node ((Topic IO) Twist)
get_pose = subscribe "/turtle1/pose"

showMsg :: P.Pose -> IO ((Float,Float))
showMsg pose = return (P._x pose, P._y pose)
