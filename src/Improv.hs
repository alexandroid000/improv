{-#LANGUAGE GADTs #-}

module Improv where

import Prelude hiding (Left, Right)
        
-- agent-centered vectors, composition is vector addition
-- ie: Low :*: High = Mid
data Direction = Lef | Righ | Forward
               | Backward | Center
               | Low | Mid | High
               | Direction :*: Direction
     deriving (Show, Eq)

data Angle = Angle Double
        deriving (Show, Eq)

data Plane = XY | YZ | XZ
        deriving (Show, Eq)

class Symmetric a where
    refl :: Plane -> a -> a

instance Symmetric Direction where
    refl plane (dir1 :*: dir2) = (refl plane dir1) :*: (refl plane dir2)
    refl YZ Lef = Righ
    refl YZ Righ = Lef
    refl YZ dir = dir
    refl XZ Forward = Backward
    refl XZ Backward = Forward
    refl XZ dir = dir
    refl XY Low = High
    refl XY High = Low
    refl XY dir = dir

instance Symmetric Angle where
    refl _ (Angle x) = Angle (-x)

data Length = Zero | Quarter | Half | ThreeFourths | Full
        deriving (Show, Eq)

-- should we have an inherent "rhythm" like Tidal does?
-- I thinks yes
type Duration = Double

-- origins indexed by integers
data Origin = O Int
            | MultOs Origins
    deriving (Show, Eq)

type Origins = [Origin]

data Action = A Origin Direction Length
            | As Actions
    deriving (Show, Eq)

type Actions = [Action]

instance Symmetric Action where
    refl pl (A o dir len) = A o (refl pl dir) len
    refl pl (As acts) = As (map (refl pl) acts)

-- super: "to which b is attached"
-- child: "parts attached to b"
-- this doesn't make sense to me
-- we really want a collection of parts to also be a part: contains/containedBy?
class Parts b where
    contains :: b -> [b]
    origin :: b -> Origin

-- should we use inductive graphs? yesss
-- binary tree with pointer to parent node
data KineChain a = Joint Origin (KineChains a)
                 | Link Origin a
                 | Collection (KineChains a)
     deriving (Show, Eq)

type KineChains a = [KineChain a]

-- can add more data constructors for different robot representations
-- ie: graphs, forests, fingertrees
type Robot a = KineChain a

instance Parts (KineChain a) where
    contains (Link o n) = [Link o n]
    contains (Joint o ks) = concatMap contains ks
    contains (Collection ks) = concatMap contains ks
    origin (Link o _) = o
    origin (Joint o _) = o
    origin (Collection ks) = MultOs $ map origin ks

type XYZ = (Double, Double, Double)

-- parameterized over parts, but we want to map over actions
-- every dance should be compilable to hardware
-- so base case should involve a "Part"
-- in general I don't like divorcing Actions from Parts
data Dance b = Prim Action b
             | Rest Duration
             | Dance b :+: Dance b -- in series
             | Dance b :||: Dance b -- in parallel
        deriving (Show, Eq)

-- combinators
--------------

seqL, parL :: (Parts a) => [Dance a] -> Dance a
seqL = foldr (:+:) (Rest 0)
parL = foldr (:||:) (Rest 0)

repeatn :: (Parts a) => Int -> Dance a -> Dance a
repeatn n dance = seqL $ take n $ repeat dance

-- transformers
---------------

-- map over parts (for changing platforms)
instance Functor Dance where
    fmap f (x :+: y) = (fmap f x) :+: (fmap f y)
    fmap f (x :||: y) = (fmap f x) :||: (fmap f y)
    fmap f (Rest dur) = Rest dur
    fmap f (Prim act part) = Prim act (f part)

-- map over all actions in a dance
transform :: (Parts a) => (Action -> Action) -> Dance a -> Dance a
transform f (x :+: y) = (transform f x) :+: (transform f y)
transform f (x :||: y) = (transform f x) :||: (transform f y)
transform f (Rest dur) = Rest dur
transform f (Prim act dur) = Prim (f act) dur


-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

