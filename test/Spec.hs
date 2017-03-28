import Improv

actions :: [Dance a]
actions = [Prim (Move Forward) 1.0, Prim (Move Backward) 1.0]



main :: IO ()
main = do
    print $ mconcat dance1
