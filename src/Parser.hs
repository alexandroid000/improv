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

type OurDance = Dance (Robot Double)
type CommandState = Map String Tree

data ParseErr = ParseErr Integer String -- line # (-1 for parsec errors) and error string
data Tree = Node [Tree]
          | Bracket [Tree]
          | Leaf String
    deriving (Show,Eq)


left = Prim (A Lef Quarter) 1
right = Prim (A Righ Quarter) 1
forward = Prim (A Forward Quarter) 1
rest = Prim (A Center Zero) 1

startCommands = Map.fromList [("left", left), ("right", right),
                                ("forward", forward), ("rest", rest)]
multiFuncs = Map.fromList [("approach", approach)] -- Dictionary of names to multiFuncs
axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]
channelNames = Map.fromList [("a1", core)] -- Dictionary of channel names to Robot Doubles

---------------------------------------------------------------------------------------------

approach :: (Robot Double) -> (Robot Double) -> OurDance
approach x y = left x --PLACEHOLDER FOR MULTIFUNC

convertFile :: String -> Either ParseErr (Topic IO Twist)
convertFile doc = case parse parseDoc "" doc of
    Right tree -> evalState (convertLines tree (Map.fromList []) 1) (Map.fromList []) >>= return . moveCommands . danceToMsg . (rest core :+:) -- rest core temporary
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error

convertLines :: Tree -> Map String OurDance -> Integer ->
                    S.State CommandState (Either ParseErr OurDance)
convertLines (Node []) channels linenum = return $ Right $ parL $ Map.elems channels 
convertLines (Node (line:lines)) channels linenum = 
    let createErr err = return $ Left $ ParseErr linenum err in
    do commandDefs <- get
       case line of
            Node [Leaf "=", Leaf var, body] -> do modify $ Map.insert var body
                                                  convertLines (Node lines) channels (linenum + 1)
            Node [Leaf "$", Leaf var, body] -> case Map.lookup var channelNames of
                Just robo -> case evalState (convertCommands robo body) commandDefs of 
                      Right dance -> convertLines (Node lines) (Map.insert var dance channels) (linenum + 1)
                      Left err -> createErr err
                Nothing -> createErr $ "Could not find robot with name " ++ var
            Node [] -> convertLines (Node lines) channels (linenum + 1)
            otherwise -> createErr $ "Incorrect syntax on " ++ show linenum
            --Node [multiFuncs] -> case convertMultiFuncs multiFuncs commandDefs of
              --  Right newCommandDefs -> do put newCommandDefs
                --Left err -> createErr err

               
convertCommands :: Robot Double -> Tree -> S.State CommandState (Either String OurDance)
----Single command lookup----
convertCommands robo (Leaf command) = do commandDefs <- get --TEST CODE!!!!
                                         case Map.lookup command startCommands of
                                              Just x -> return $ Right $ x robo
                                              Nothing -> case Map.lookup command commandDefs of
                                                              Just xx -> convertCommands robo xx
                                                              Nothing -> return $ Left ("Invalid command: " ++ command)
----Node base cases----                                   
convertCommands robo (Node []) = return $ Right Skip
----repeat commands or repeat n commands
convertCommands robo (Node [Leaf "repeat", x])  = convertCommands robo x >>= \eitherDance -> return $ eitherDance >>= return . seqL . repeat
convertCommands robo (Node [Leaf "repeat", Leaf numStr, x]) = case reads numStr of
    [(num, [])] ->  convertCommands robo x >>= \eitherDance -> return $ eitherDance >>= return . repeatn num
    otherwise -> throwErr $ "Invalid argument to repeat: " ++ numStr
----reflect axis commands----
convertCommands robo (Node [Leaf "reflect", Leaf ax, x]) = case Map.lookup ax axes of
    Just axis -> convertCommands robo x >>= \eitherDance -> return $ eitherDance >>= return --TEMP CODE: reflect does not currently work on dance
    Nothing -> throwErr $ "Invalid argument to reflect: " ++ ax
----List of commands----
convertCommands robo (Node xx) = case Split.splitOn [Leaf "||"] xx of -- check if any parallel chunks
    [xx] -> mapM (convertCommands robo) xx >>= \eitherDances -> return $ mapM id eitherDances >>= return . foldr (:+:) Skip
    xxs -> mapM (convertCommands robo) (map Node xxs) >>= \eitherDances -> return $ mapM id eitherDances >>= return . parL
----Sequenced commands----
convertCommands robo (Bracket xx) = mapM (convertCommands robo) xx >>= \eitherDances -> return $ mapM id eitherDances >>= return . seqL
convertCommands _ _ = throwErr "Invalid arguments."

throwErr :: String -> S.State CommandState (Either String OurDance)
throwErr err = return $ Left err

--convertMultiFuncs (Node [Leaf xx, Leaf channel1, Leaf channel2]) = 
--convertMultiFuncs (Node (Node xx):xs) = convertMultiFuncs $ Node xx

----Parsec parsers----
skipSpace :: Parser ()
skipSpace = skipMany (char ' ')

varName :: Parser String
varName = do var1 <- letter
             var2 <- many (digit <|> letter)
             return $ var1:var2

parseDoc :: Parser Tree
parseDoc = Node <$> sepEndBy (skipSpace >> Node <$> ((try parseLine) <|> parseNode)) newline where
    parseNode = many ((parseParens <|> parseBracket <|> parseLeaf) >>= \t -> skipSpace >> return t)
    parseParens = Node <$> between (char '(') (char ')') parseNode
    parseBracket = Bracket <$> between (char '[') (char ']') parseNode
    parseLeaf = Leaf <$> (try (many1 (digit <|> letter)) <|> string "||")
    parseLine  = do vars <- many1 (varName >>= \x -> skipSpace >> return x)
                    opType <- oneOf "=$"
                    skipSpace
                    body <- parseNode
                    return $ [Leaf [opType]] ++ (map Leaf vars) ++ [Node body]
