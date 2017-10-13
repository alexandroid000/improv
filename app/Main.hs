{-# LANGUAGE OverloadedStrings #-}

import Ros.Node
import Ros.Topic.Util (topicRate)
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

main :: IO ()
main = do doc <- readFile "./src/test.imp"
          case convertFile doc of 
               Right moveTopics -> runNode "HaskellTurtle" $
                     do pose1 <- subscribe "/turtle1/pose" 
                        pose2 <- subscribe "/turtle2/pose"
                        mapM (\(name, twist) -> advertise (name ++ "/cmd_vel") (topicRate robotRes twist)) $ Map.toList moveTopics
               Left (ParseErr (-1) err) -> putStrLn $ "Parse error " ++ err
               Left (ParseErr line err) -> putStrLn $ "Error (line " ++ (show line) ++ "):\n" ++ err
