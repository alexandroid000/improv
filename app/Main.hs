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

-- robotRate, moveTopic should be defined in RobotSpec module

main :: IO ()
main = do args <- getArgs
          case args of
              [] -> do doc <- readFile "./src/test.imp"
                       case convertFile doc of 
                            Right moveTopics ->  runNode "HaskellTurtle" 
                            						$ mapM (\(name, twist) -> advertise (name ++ "/cmd_vel") (topicRate robotRes twist)) 
                            						$ Map.toList moveTopics
                            Left (ParseErr (-1) err) -> putStrLn $ "Parse error " ++ err
                            Left (ParseErr line err) -> putStrLn $ "Error (line " ++ (show line) ++ "):\n" ++ err
              _  -> putStrLn "Incorrect number of arguments: need file name"
