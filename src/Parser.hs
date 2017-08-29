module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)
import Text.ParserCombinators.Parsec 
import Control.Monad.State.Lazy as S
import qualified Data.List.Split as Split

type OurDance = Dance (KineChain Double)
type CommandState = Map String OurDance

data ParseErr = ParseErr Integer String -- line # (-1 for parsec errors) and error string
data Tree = Node [Tree]
          | Bracket [Tree]
          | Leaf String
    deriving (Show,Eq)


left = Prim (A Lef Quarter) 1 core
right = Prim (A Righ Quarter) 1 core
forward = Prim (A Forward Quarter) 1 core

startCommands = Map.fromList [("left", left), ("right", right), ("forward", forward)]
axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]

parseFile :: String -> Either ParseErr (Topic IO Twist)
parseFile doc = case parse parseDoc "" doc of
    Right tree -> evalState (parseLines tree (Map.fromList []) 1) startCommands >>= return . moveCommands . danceToMsg
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error

parseLines :: Tree -> Map String OurDance -> Integer ->
                    S.State CommandState (Either ParseErr OurDance)
parseLines (Node []) channels linenum = return $ Right $ parL $ Map.elems channels 
parseLines (Node (line:lines)) channels linenum = 
    do commands <- get
       case line of
            Node [Leaf "=", Leaf var, body] -> case evalState (parseWords body) commands of
                Right dance -> do modify $ Map.insert var dance
                                  parseLines (Node lines) channels (linenum + 1)
                Left err -> return $ Left $ ParseErr linenum err
            Node [Leaf "$", Leaf var, body] -> case evalState (parseWords body) commands of 
                Right dance -> parseLines (Node lines) (Map.insert var dance channels) (linenum + 1)
                Left err -> return $ Left $ ParseErr linenum err

               
parseWords :: Tree -> S.State CommandState (Either String OurDance)
----Single command lookup----
parseWords (Leaf command) = do commands <- get --TEST CODE!!!!
                               case Map.lookup command commands of
                                   Just stuff -> return $ Right stuff
                                   Nothing -> return $ Left ("Invalid command: " ++ command)
----Node base cases----                                   
parseWords (Node []) = return $ Right Skip
----repeat commands or repeat n commands
parseWords (Node [Leaf "repeat", x]) = parseWords x >>= \e -> return $ e >>= return . seqL . repeat
parseWords (Node [Leaf "repeat", Leaf numStr, x]) = case reads numStr of
    [(num, [])] ->  parseWords x >>= \e -> return $ e >>= return . repeatn num
    otherwise -> throwErr $ "Invalid argument to repeat: " ++ numStr
----reflect axis commands----
parseWords (Node [Leaf "reflect", Leaf ax, x]) = case Map.lookup ax axes of
    Just axis -> parseWords x >>= \e -> return $ e >>= return --TEMP CODE: reflect does not currently work on dance
    Nothing -> throwErr $ "Invalid argument to reflect: " ++ ax
----List of commands----
parseWords (Node xx) = case Split.splitOn [Leaf "||"] xx of -- check if any parallel chunks
    [xx] -> mapM parseWords xx >>= \ee -> return $ mapM id ee >>= return . foldr (:+:) Skip
    xxs -> mapM parseWords (map Node xxs) >>= \ee -> return $ mapM id ee >>= return . parL
----Sequenced commands----
parseWords (Bracket xx) = mapM parseWords xx >>= \ee -> return $ mapM id ee >>= return . seqL
parseWords _ = throwErr "Invalid arguments."

throwErr :: String -> S.State CommandState (Either String OurDance)
throwErr err = return $ Left err

----Parsec parsers----
skipSpace :: Parser ()
skipSpace = skipMany (char ' ')

parseDoc :: Parser Tree
parseDoc = Node <$> sepEndBy (skipSpace >> Node <$> parseLine) newline where
    parseNode = many ((parseParens <|> parseBracket <|> parseLeaf) >>= \t -> skipSpace >> return t)
    parseParens = Node <$> between (char '(') (char ')') parseNode
    parseBracket = Bracket <$> between (char '[') (char ']') parseNode
    parseLeaf = Leaf <$> (try (many1 (digit <|> letter)) <|> string "||")
    parseLine  = do var1 <- letter
                    var2 <- many (digit <|> letter)
                    skipSpace
                    opType <- oneOf "=$"
                    skipSpace
                    body <- parseNode
                    return [Leaf [opType], Leaf (var1:var2), Node body]
