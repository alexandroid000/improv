{-#LANGUAGE GADTs #-}

module Improv where

import Prelude
import Algebra.Ring (C)
        
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

data Origin = Origin
    deriving (Show, Eq)


data Action = A Origin Direction Length
    deriving (Show, Eq)

-- super: "to which b is attached"
-- child: "parts attached to b"
-- this doesn't make sense to me
-- we really want a collection of parts to also be a part: contains/containedBy?
class Parts b where
    contains :: b -> [b]
    containedBy :: b -> [b]
    symmetricPart :: b -> b
    size :: b -> XYZ
    jointAt :: b -> b -> Maybe (XYZ, XYZ)
    origin :: b -> b

-- should we use inductive graphs? yesss
-- binary tree with pointer to parent node
data KineChain a = Root a
                 | Link (KineChain a) a
                 | Joint (KineChain a) (KineChain a) (KineChain a)
                 | Collection (KineChain a) (KineChain a)
     deriving (Show, Eq)

-- can add more data constructors for different robot representations
-- ie: graphs, forests, fingertrees
type Robot a = KineChain a


type XYZ = (Double, Double, Double)

-- parameterized over parts, but we want to map over actions
-- every dance should be compilable to hardware
-- so base case should involve a "Part"
-- in general I don't like divorcing Actions from Parts
data Dance b = Prim Action (Robot Int)
             | Rest Duration
             | WithPart b (Dance b)
             | Dance b :+: Dance b -- in series
             | Dance b :=: Dance b -- in parallel
        deriving (Show, Eq)

-- combinators
--------------

seqL, parL :: (Parts a) => [Dance a] -> Dance a
seqL = foldr (:+:) (Rest 0)
parL = foldr (:=:) (Rest 0)

repeatn :: (Parts a) => Int -> Dance a -> Dance a
repeatn n dance = seqL $ take n $ repeat dance

-- transformers
---------------

-- map over parts (for changing platforms)
instance Functor Dance where
    fmap f (x :+: y) = (fmap f x) :+: (fmap f y)
    fmap f (x :=: y) = (fmap f x) :=: (fmap f y)
    fmap f (WithPart p dance) = WithPart (f p) (fmap f dance)
    fmap f (Rest dur) = Rest dur
    fmap f (Prim act dur) = Prim act dur

-- map over all actions in a dance
transform :: (Parts a) => (Action -> Action) -> Dance a -> Dance a
transform f (x :+: y) = (transform f x) :+: (transform f y)
transform f (x :=: y) = (transform f x) :=: (transform f y)
transform f (WithPart p dance) = WithPart p (transform f dance)
transform f (Rest dur) = Rest dur
transform f (Prim act dur) = Prim (f act) dur



--changeDir :: Direction -> Action -> Action
--changeDir dir (Move _) = Move dir
--changeDir _ act = act


