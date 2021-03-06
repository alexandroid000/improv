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
import Data.List
import Debug.Trace

type OurRobot = Robot Double
type OurDance = Dance (OurRobot)
type CommandState = Map String Tree

data ParseErr = ParseErr Integer String -- line # (-1 for parsec errors) and error string
data Tree = Node [Tree]
          | Bracket [Tree]
          | Leaf String
    deriving (Show,Eq)


left      = Prim (A Lef Quarter) 1
halfleft  = Prim (A Lef Eighth) 1
right     = Prim (A Righ Quarter) 1
halfright = Prim (A Righ Eighth) 1
forward   = Prim (A Forward Full) 1
backward  = Prim (A Backward Full) 1
rest      = Prim (A Center Zero) 1

-- motion primitives
startCommands = Map.fromList [
                    ("left", left)
                  , ("right", right)
                  , ("halfleft", halfleft)
                  , ("halfright", halfright)
                  , ("forward", forward)
                  , ("backward", backward)
                  , ("rest", rest)]

multiFuncs = Map.fromList [("approach", approach)] -- Dictionary of names to multiFuncs
axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]

-- Dictionary of channel names to OurRobots
channelNames = Map.fromList [
                ("turtle1", core)
              , ("turtle2", core)
              , ("turtle3", core)
              , ("turtle4", core)]

---------------------------------------------------------------------------------------------
approach :: [OurRobot] -> Either String [OurDance]
approach [x, y] = Right [rest x :+: rest x :+: repeatn 10 (forward x), left y :+: left y :+: repeatn 10 (forward y)] --VERY BASIC MULTIFUNC

-- adds rest at beginning of dance to allow time for ROS to initialize
-- could cause problem if overall RobotRate is too low (rest is too short)
convertFile :: String -> Either ParseErr (Map String (Topic IO Twist))
convertFile doc = case parse parseDoc "" doc of
    Right (Node ((Leaf beatStr):tree)) -> aux (read beatStr :: Int) (Node tree)
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error
    where aux beat tree = evalState (convertLines tree (Map.fromList []) 1) (Map.fromList []) >>= 
              return . Map.map moveCommands . Map.map danceToMsg . Map.map (changeTiming ((fromIntegral beat) / 60.0)) . Map.map (rest core :+:)

convertLines :: Tree -> Map String OurDance -> Integer ->
                    S.State CommandState (Either ParseErr (Map String OurDance))
convertLines (Node []) channels linenum = return $ Right channels
    where test = parL [(right core), (forward core)]
convertLines (Node (line:lines)) channels linenum = 
    let createErr err = return $ Left $ ParseErr linenum err in
    do commandDefs <- get
       case line of
            Node [] -> convertLines (Node lines) channels (linenum + 1) -- Empty line
            Node [Leaf "=", Leaf var, body] -> do modify $ Map.insert var body -- Variable assignment
                                                  convertLines (Node lines) channels (linenum + 1)
            Node [Leaf "$", Node vars, body] -> case mapM (\(Leaf var) -> Map.lookup var channelNames) vars of
                Just robos -> case evalState (convertCommands robos body) commandDefs of
                    Right dances -> convertLines (Node lines) 
                        (Map.unionWith (\oldDance -> \newDance -> oldDance :+: newDance) (Map.fromList (zip (map (\(Leaf var) -> var) vars) dances)) channels) (linenum + 1)
                    Left err -> createErr err
                Nothing -> createErr $ "Could not find robot with given name in line " ++ show linenum
            otherwise -> createErr $ "Could not pattern match syntax tree: " ++ show line

               
convertCommands :: [OurRobot] -> Tree -> S.State CommandState (Either String [OurDance])
----Single command lookup----
convertCommands robos (Leaf commandStr) =
    do commandDefs <- get --TEST CODE!!!!
       case Map.lookup commandStr startCommands of
            Just command -> return $ Right $ map command robos
            Nothing -> case Map.lookup commandStr commandDefs of
                Just commands -> convertCommands robos commands
                Nothing -> case Map.lookup commandStr multiFuncs of
                    Just multiFunc -> return $ multiFunc robos
                    Nothing -> return $ Left ("Invalid command: " ++ commandStr)
----Node base cases----                                   
convertCommands robos (Node []) = return $ Right $ take (length robos) $ repeat Skip
----repeat commands or repeat n commands
convertCommands robos (Node [Leaf "repeat", x])  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (foldr (:+:) Skip) . map repeat
convertCommands robos (Node [Leaf "repeat", Leaf numStr, x]) = case reads numStr of
    [(num, [])] ->  convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (repeatn num)
    otherwise -> throwErr $ "Invalid argument to repeat: " ++ numStr
----reflect axis commands----
convertCommands robos (Node [Leaf "reflect", Leaf axStr, x]) = case Map.lookup axStr axes of
    Just axis -> convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (transform (refl axis))
    Nothing -> throwErr $ "Invalid argument to reflect: " ++ axStr
----Reverse dances----
convertCommands robos (Node [Leaf "reverse", x])  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map reverseDance
----Retrograde dances----
convertCommands robos (Node [Leaf "retrograde", x])  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map retrogradeDance
----List of commands----
convertCommands robos (Node xx) = case Split.splitOn [Leaf "||"] xx of -- check if any parallel chunks
    [comm] -> mapM (convertCommands robos) comm >>= \eitherDances -> return $ mapM id eitherDances >>= return . foldr (zipWith (:+:)) (take (length robos) (repeat Skip))
    comms -> mapM (convertCommands robos) (map Node comms) >>= \eitherDances -> return $ mapM id eitherDances >>= return . map parL . transpose . map reverse
----Sequenced commands----
convertCommands robos (Bracket xx) = mapM (convertCommands robos) xx >>= \eitherDances -> return $ mapM id eitherDances >>= return . map seqL . transpose . map reverse

throwErr :: String -> S.State CommandState (Either String [OurDance])
throwErr err = return $ Left err

--convertMultiFuncs (Node [Leaf xx, Leaf channel1, Leaf channel2]) = 
--convertMultiFuncs (Node (Node xx):xs) = convertMultiFuncs $ Node xx

----Parsec parsers----
skipSpace :: Parser ()
skipSpace = skipMany (char ' ')

skipNewline :: Parser ()
skipNewline = skipMany (char '\n')

varName :: Parser String
varName = do var1 <- letter
             var2 <- many (digit <|> letter)
             return $ var1:var2

parseDoc :: Parser Tree
parseDoc = Node <$> do beat <- parseBeat
                       tree <- many (skipNewline >> skipSpace >> Node <$> ((try parseAssign) <|> parseLine) >>= \x -> skipNewline >> return x)
                       return (beat:tree)
    where
    parseNode = many ((parseParens <|> parseBracket <|> parseLeaf) >>= \t -> skipSpace >> return t)
    parseParens = Node <$> between (char '(') (char ')') parseNode
    parseBracket = Bracket <$> between (char '[') (char ']') parseNode
    parseLeaf = Leaf <$> (try (many1 (digit <|> letter)) <|> string "||")
    parseAssign = do var <- varName
                     skipSpace
                     char '='
                     skipSpace
                     body <- parseNode
                     return $ [Leaf "=", Leaf var, Node body]
    parseLine = do vars <- many1 (varName >>= \x -> skipSpace >> return x)
                   char '$'
                   skipSpace
                   body <- parseNode
                   return $ [Leaf "$", Node (map Leaf vars), Node body]
    parseBeat = do string "beat"
                   skipSpace
                   beat <- many1 digit
                   return $ Leaf beat
