module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import Data.List.Split
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)
import Text.ParserCombinators.Parsec 
import Control.Monad.State.Lazy as S

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
parseFile doc = case parse parseDoc "" doc of
    Right tree -> evalState (parseLines tree [] 1) startenv >>= return . moveCommands . map (move core)
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error
    

parseLines :: Tree -> Actions -> Integer -> S.State (Map String Actions) (Either ParseErr Actions)
parseLines (Node []) acc linenum = return $ Right acc
parseLines (Node (line:lines)) acc linenum = 
    do env <- get
       case line of
           Node [Leaf [v, "="], body] -> case evalState (parseWords body) env of
               Right acts -> do modify $ Map.insert v acts
                                parseLines (Node lines) acc (linenum + 1)
               Left err -> return $ Left $ ParseErr linenum err
           otherwise -> case evalState (parseWords line) env of 
               Right acts -> parseLines (Node lines) (acc ++ acts) (linenum + 1)
               Left err -> return $ Left $ ParseErr linenum err
               
parseWords :: Tree -> S.State (Map String Actions) (Either String Actions) 
parseWords curr = do env <- get --TEST CODE!!!!
                     case Map.lookup "left" env of
                        Just stuff -> return $ Right $ actrepeat stuff
                        Nothing -> return $ Left ""

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
          
