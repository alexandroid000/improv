module Main where

import Test.Hspec
import Test.QuickCheck


import Improv
import RobotSpec

robot = Link (O 0) 0
left = A Lef Quarter
right = A Righ Zero
d1 = Prim left robot
d2 = Prim right robot
dance = [d1, d2, d1]

main :: IO ()
main = hspec $ do
    describe "Improv.hs" $ do
        it "parallel" $
            (getPar . mconcat $ map ParDance dance)
            `shouldBe`
            (Prim (A Lef Quarter) (Link (O 0) 0)    :||:
            (Prim (A Righ Zero) (Link (O 0) 0)      :||:
            (Prim (A Lef Quarter) (Link (O 0) 0)    :||:
            Rest)))
        it "series" $
            (getSeq . mconcat $ map SeqDance dance)
            `shouldBe`
            (Prim (A Lef Quarter) (Link (O 0) 0)    :+:
            (Prim (A Righ Zero) (Link (O 0) 0)      :+:
            (Prim (A Lef Quarter) (Link (O 0) 0)    :+:
            Skip)))
--    print $ mconcat $ map SeqDance dance
