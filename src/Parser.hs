module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)
import Text.ParserCombinators.Parsec 
import Control.Monad.State.Lazy as S

type CommandState = Map String Actions

data ParseErr = ParseErr Integer String -- line # (-1 for parsec errors) and error string
data Tree = Node [Tree]
          | Leaf String
    deriving Show


left = A (origin core) Lef Quarter
right = A (origin core) Righ Zero
forward = A (origin core) Forward Quarter

startCommands = Map.fromList [("left", [left]), ("right", [right]), ("forward", [forward])]
axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]

parseFile :: String -> Either ParseErr (Topic IO Twist)
parseFile doc = case parse parseDoc "" doc of
    Right tree -> evalState (parseLines tree [] 1) startCommands >>= return . moveCommands . map (move core)
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error

parseLines :: Tree -> Actions -> Integer -> S.State CommandState (Either ParseErr Actions)
parseLines (Node []) acc linenum = return $ Right acc
parseLines (Node (line:lines)) acc linenum = 
    do commands <- get
       case line of
            Node [Leaf v, Leaf "=", body] -> case evalState (parseWords body) commands of
                Right acts -> do modify $ Map.insert v acts
                                 parseLines (Node lines) acc (linenum + 1)
                Left err -> return $ Left $ ParseErr linenum err
            otherwise -> case evalState (parseWords line) commands of 
                Right acts -> parseLines (Node lines) (acc ++ acts) (linenum + 1)
                Left err -> return $ Left $ ParseErr linenum err
               
parseWords :: Tree -> S.State CommandState (Either String Actions) 
parseWords curr = do env <- get --TEST CODE!!!!
                     case Map.lookup "left" env of
                        Just stuff -> return $ Right $ actrepeat stuff
                        Nothing -> return $ Left ""

----Helper Functions----

actrepeat :: Actions -> Actions
actrepeat acts = acts ++ actrepeat acts

actrepeatn :: Int -> Actions -> Actions
actrepeatn 0 acts = []
actrepeatn n acts = acts ++ actrepeatn (n - 1) acts

skipSpace :: Parser ()
skipSpace = skipMany (char ' ')

parseDoc :: Parser Tree
parseDoc = Node <$> sepEndBy (skipSpace >> (try parseLet <|> parseLine)) newline where
    parseLine = Node <$> many1 ((parseNode <|> parseLeaf) >>= \t -> skipSpace >> return t)
    parseNode = between (char '(') (char ')') parseLine
    parseLeaf = Leaf <$> many1 (digit <|> letter)
    parseLet  = do v1 <- letter
                   v2 <- many (digit <|> letter)
                   skipSpace
                   char '='
                   skipSpace
                   body <- parseLine
                   return $ Node [Leaf (v1:v2), Leaf "=", body]
          
