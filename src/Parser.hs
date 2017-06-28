module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import Data.List.Split
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)

data ParseErr = ParseErr Integer String -- line # and error string

left = A (origin core) Lef Quarter
right = A (origin core) Righ Zero
forward = A (origin core) Forward Quarter

startenv = [("left", [left]), ("right", [right]), ("forward", [forward])]

parseFile :: String -> Either ParseErr (Topic IO Twist)
parseFile doc = parseLines (splitOn "\n" doc) (Map.fromList startenv) 1 >>= \acts -> return $ moveCommands $ move core $ As acts

parseLines :: [String] -> Map String Actions -> Integer -> Either ParseErr Actions
parseLines [] env linenum = Right []
parseLines (line:lines) env linenum = aux (words line) where
    aux (v:"=":body) =
        case parseWords body env of
            Just acts -> parseLines lines (Map.insert v acts env) (linenum + 1)
            Nothing -> Left $ ParseErr linenum line
    aux body = 
        case parseWords body env of
            Just acts -> parseLines lines env (linenum + 1) >>= \rest -> return $ acts ++ rest
            Nothing -> Left $ ParseErr linenum line

parseWords :: [String] -> Map String Actions -> Maybe Actions
parseWords [] env = Just []
parseWords (curr:rest) env = Map.lookup curr env >>= \acts -> parseWords rest env >>= \restacts -> return $ acts ++ restacts
