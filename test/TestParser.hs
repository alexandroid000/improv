module TestParser where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import Parser

f1 = "beat 20\n\nx = left right\n\nturtle1 $ x"
f2 = "beat 20\n\nx = left right\n\nturtle1 turtle2 $ x"

parser_tests = describe "Parser.hs" $ do
        it "basic parse" $
            parse parseDoc "" f1
            `shouldBe`
            Right (Node [Leaf "20",Node [Leaf "=",Leaf "x",Node [Leaf
            "left",Leaf "right"]],Node [Leaf "$",Node [Leaf "turtle1"],Node
            [Leaf "x"]]])
        it "multirob parse" $
            parse parseDoc "" f2
            `shouldBe`
            Right (Node [Leaf "20",Node [Leaf "=",Leaf "x",Node [Leaf "left",Leaf "right"]],Node [Leaf "$",Node [Leaf "turtle1",Leaf "turtle2"],Node [Leaf "x"]]])
