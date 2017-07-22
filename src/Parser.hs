module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import Data.List.Split
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)
import Text.Read
import Text.ParserCombinators.Parsec 

data ParseErr = ParseErr Integer String -- line # and error string
data Tree = Node [Tree]
          | Leaf [String]
    deriving Show

left = A (origin core) Lef Quarter
right = A (origin core) Righ Zero
forward = A (origin core) Forward Quarter

startenv = Map.fromList [("left", [left]), ("right", [right]), ("forward", [forward])]
axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]

parseFile :: String -> Either ParseErr (Topic IO Twist)
parseFile doc = case parse parseDoc "lisp" doc of
    Right tree -> parseLines tree startenv 1 >>= return . moveCommands . move core . As
    Left err -> Left $ ParseErr 0 (show err)

parseLines :: Tree -> Map String Actions -> Integer -> Either ParseErr Actions
parseLines (Node []) env linenum = Right []
parseLines (Node (line:lines)) env linenum = case line of
    Node [Leaf [v, "="], body]   -> parseWords body env linenum >>= \acts -> parseLines (Node lines) (Map.insert v acts env) (linenum + 1)
    otherwise                    -> parseWords line env linenum >>= \acts -> parseLines (Node lines) env (linenum + 1) >>= \rest -> return $ acts ++ rest

parseWords :: Tree -> Map String Actions -> Integer -> Either ParseErr Actions
parseWords curr env linenum = case curr of
    otherwise -> Right []
    
--parseWords (Leaf ("":rest)) env linenum = parseWords (Leaf rest) env linenum
--parseWords ("reflect":arg1:arg2:rest) env linenum = parseWords [arg2] env linenum >>= \acts -> 
--    case Map.lookup arg1 axes of
--        Just ax -> parseWords rest env linenum >>= \restacts -> return $ (map (refl ax) acts) ++ restacts
--        Nothing -> Left $ ParseErr linenum "Invalid input axis"
--parseWords ("repeat":arg:rest) env linenum = case readMaybe arg :: Maybe Int of
--    Just num -> parseWords [head rest] env linenum >>= \acts -> parseWords (tail rest) env linenum >>= \restacts -> return $ (actrepeatn num acts) ++ restacts
--    Nothing -> parseWords [arg] env linenum >>= \acts -> parseWords rest env linenum >>= \restacts -> return $ (actrepeat acts) ++ restacts
--parseWords (Node ((Node n):rest)) env linenum = parseWords n env linenum >>= \acts -> parseWords (Node rest) env linenum >>= \restacts -> return $ acts ++ restacts
--parseWords (Node ((Leaf word):rest)) env linenum = case Map.lookup word env of
--    Just acts -> parseWords rest env linenum >>= \restacts -> return $ acts ++ restacts
 --   Nothing -> Left $ ParseErr linenum ("Variable lookup failed on: " ++ word)
--parseWords _ env linenum = Right []
        
----Helper Functions----

actrepeat :: Actions -> Actions
actrepeat acts = acts ++ actrepeat acts

actrepeatn :: Int -> Actions -> Actions
actrepeatn 0 acts = []
actrepeatn n acts = acts ++ actrepeatn (n - 1) acts

parseDoc :: Parser Tree
parseDoc = Node <$> sepEndBy ((try parseLet) <|> parseLine) newline
    where parseLine = Node <$> many1 (parseLeaf <|> parseNode)
          parseNode = do skipMany (char ' ')
                         stuff <- between (char '(') (char ')') parseLine
                         skipMany (char ' ')
                         return stuff
          parseLeaf = Leaf <$> words <$> many1 (digit <|> letter <|> char ' ')
          parseLet  = do v1 <- letter
                         v2 <- many (digit <|> letter)
                         skipMany (char ' ')
                         char '='
                         skipMany (char ' ')
                         body <- parseLine
                         return $ Node [Leaf [(v1:v2), "="], body]
          
