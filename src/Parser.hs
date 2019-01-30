module Parser where

import RobotSpec
import Improv
import Data.Map (Map)
import qualified Data.Map as Map
import Ros.Geometry_msgs.Twist
import Ros.Topic (Topic)
import qualified Data.List.Split as Split
import Data.List
--import Debug.Trace

-- parser
import Control.Monad.State.Lazy as S
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

type OurRobot = Robot Double
type OurDance = Dance (OurRobot)
type CommandState = Map String Tree

data ParseErr = ParseErr Integer String -- line # (-1 for parsec errors) and error string
data Expr = Var String | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
data Unop = Reverse | Retrograde | Repeat | Reflect
    deriving Show
data Duop = Sequ | Para | RepeatN
    deriving Show
data Stmt = String := Expr | Beat String | RoboCmd RoboName Expr
    deriving Show
type RoboName = String

data Tree = Node [Tree]
          | Bracket [Tree]
          | Leaf String
    deriving (Show,Eq)

def = emptyDef{   identStart = alphaNum
		, identLetter = alphaNum
		, opStart = oneOf "|=$"
		, reservedOpNames = ["||","=","reverse","retrograde","repeat","repeatN","reflect"]
		, reservedNames = ["beat"]
		}

TokenParser{ parens = m_parens
	   , identifier = m_identifier
	   , reservedOp = m_reservedOp
	   , reserved = m_reserved
	   , whiteSpace = m_whiteSpace
	   , brackets = m_brackets
	   } = makeTokenParser def


exprparser :: Parser Expr
exprparser = choice [ parseRepeatN
		    , (buildExpressionParser table term <?> "expression")
		    ]
table = [ [Prefix (m_reservedOp "reverse" >> return (Uno Reverse))]
	, [Prefix (m_reservedOp "retrograde" >> return (Uno Retrograde))]
	, [Prefix (m_reservedOp "repeat" >> return (Uno Repeat))]
	, [Prefix (m_reservedOp "reflect" >> return (Uno Reflect))]
	, [Infix  (m_reservedOp "||" >> return (Duo Para)) AssocLeft]
	]
term =  m_parens exprparser
	<|> m_brackets exprparser
	<|> fmap Var m_identifier

parseRepeatN :: Parser Expr
parseRepeatN = do { m_reservedOp "repeatN"
                  ; n <- exprparser
                  ; exp <- exprparser
                  ; return $ Duo RepeatN n exp
                  }

parseBeat :: Parser Stmt
parseBeat = do { m_reserved "beat"
               ; m_whiteSpace
               ; beat <- many1 digit
               ; return $ (Beat beat)
               }

parseVarAssign :: Parser Stmt
parseVarAssign = do { v <- m_identifier
                    ; m_reservedOp "="
                    ; e <- exprparser
                    ; return (v := e)
                    }


parseDance :: Parser Stmt
parseDance = do { v <- m_identifier
              	; m_reservedOp "$"
               	; e <- exprparser
               	; return (RoboCmd v e)
               	}

--many (skipNewline >> skipSpace >> Node <$> ((try parseAssign) <|> parseLine))
mainparser :: Parser [Stmt]
mainparser = do m_whiteSpace
		beat <- try parseBeat
		rest <- (many (skipNewline >> stmtparser) <* eof)
		return $ beat:rest
    where stmtparser :: Parser Stmt
          stmtparser = try parseVarAssign
                   <|> try parseDance


f3 = "beat 20\n\nturtle1 $ left forward || right" -- no grouping around ||
f4 = "beat 20\n\nturtle1 $ left forward || (right left)" -- grouping right of ||
f5 = "beat 20\n\nturtle1 $ (left forward) || right" -- grouping left of ||
f6 = "beat 20\n\nturtle1 $ right" -- no space

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

axes = Map.fromList [("XZ", XZ), ("XY", XY), ("YZ", YZ)]

-- Dictionary of channel names to OurRobots
channelNames = Map.fromList [
                ("turtle1", core)
              , ("turtle2", core)
              , ("turtle3", core)
              , ("turtle4", core)]

---------------------------------------------------------------------------------------------

-- adds rest at beginning of dance to allow time for ROS to initialize
-- could cause problem if overall RobotRate is too fast (rest is too short)
convertFile :: String -> Either ParseErr (Map String (Topic IO Twist))
convertFile doc = case parse mainparser "" doc of
    Right (line:lines) -> case line of
        		    (Beat beatStr) -> aux (read beatStr :: Int) lines
        		    _ -> aux 30 lines -- default beat
    Left err -> Left $ ParseErr (-1) (show err) -- Handling parsec error
    where aux beat tree = Left $ ParseErr (-1) "blah"
--    where aux beat tree = evalState (convertLines tree (Map.fromList []) 1) (Map.fromList []) >>=
--              return . Map.map moveCommands . Map.map danceToMsg . Map.map (changeTiming ((fromIntegral beat) / 60.0)) . Map.map (rest core :+:)

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
                Nothing -> return $ Left ("Invalid command: " ++ commandStr)

----Node base cases----
--convertCommands robos [] = return $ Right $ take (length robos) $ repeat Skip
----repeat commands or repeat n commands
--convertCommands robos (Uno Repeat x)  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (foldr (:+:) Skip) . map repeat
--convertCommands robos ((Uno RepeatN numStr) x) = case reads numStr of
--    [(num, [])] ->  convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (repeatn num)
--    _ -> throwErr $ "Invalid argument to repeat: " ++ numStr
----reflect axis commands----
convertCommands robos (Node [Leaf "reflect", Leaf axStr, x]) = case Map.lookup axStr axes of
    Just axis -> convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map (transform (refl axis))
    Nothing -> throwErr $ "Invalid argument to reflect: " ++ axStr
----Reverse dances----
convertCommands robos (Node [Leaf "reverse", x])  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map reverseDance
----Retrograde dances----
convertCommands robos (Node [Leaf "retrograde", x])  = convertCommands robos x >>= \eitherDances -> return $ eitherDances >>= return . map retrogradeDance
----Parallel dances---- why does this map reverse??
--convertCommands robos (Par [l,r]) = mapM (convertCommands robos) [Node [l], Node [r]] >>= \eitherDances -> return $ mapM id eitherDances >>= return . map parL . transpose . map reverse
----Normal serial dances----
convertCommands robos (Node xx) = mapM (convertCommands robos) xx >>= \eitherDances -> return $ mapM id eitherDances >>= return . foldr (zipWith (:+:)) (take (length robos) (repeat Skip))
----Sequenced commands, compressed time----
convertCommands robos (Bracket xx) = mapM (convertCommands robos) xx >>= \eitherDances -> return $ mapM id eitherDances >>= return . map seqL . transpose . map reverse

throwErr :: String -> S.State CommandState (Either String [OurDance])
throwErr err = return $ Left err

--convertMultiFuncs (Node [Leaf xx, Leaf channel1, Leaf channel2]) = 
--convertMultiFuncs (Node (Node xx):xs) = convertMultiFuncs $ Node xx

----Parsec parsers----
skipNewline :: Parser ()
skipNewline = skipMany (char '\n')

varName :: Parser String
varName = do var1 <- letter
             var2 <- many (digit <|> letter)
             return $ var1:var2

--parseBracket :: Parser Tree
--parseBracket = Bracket <$> between (char '[') (char ']') parseNode

--parseParens :: Parser Tree
--parseParens = Node <$> between (char '(') (char ')') parseNode

--parseNode :: Parser [Tree]
--parseNode = many ((parseParens <|> parseBracket <|> parseLeaf) >>= \t -> skipSpace >> return t)

--parsePar :: Parser Tree
--parsePar = do l <- parseNode
--              string "||"
--              r <- parseNode
--  	      return $ Par [l,r]

--parseLeaf :: Parser Tree
--parseLeaf = Leaf <$> (try (many1 (digit <|> letter)))

--parseDoc :: Parser Tree
--parseDoc = Node <$> do beat <- parseBeat
--                       tree <- many (skipNewline >> skipSpace >> Node <$> ((try parseAssign) <|> parseLine) >>= \x -> skipNewline >> return x)
--                       return (beat:tree)
--    where  parseAssign = do var <- varName
--                            skipSpace
--                            char '='
--                            skipSpace
--                            body <- parseNode
--                            return $ [Leaf "=", Leaf var, Node body]
--           parseLine = do vars <- many1 (varName >>= \x -> skipSpace >> return x)
--                          char '$'
--                          skipSpace
--                          body <- parseNode
--                          return $ [Leaf "$", Node (map Leaf vars), Node body]
--           parseBeat = do string "beat"
--                          skipSpace
--                          beat <- many1 digit
--                          return $ Leaf beat

play :: String -> IO ()
play inp = case parse mainparser "" inp of
		Left err -> print err
		Right ans -> print ans
