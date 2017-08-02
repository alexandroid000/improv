{-#LANGUAGE GADTs #-}
{-#LANGUAGE DeriveFunctor #-}

module Improv where

import Prelude
        
-- agent-centered vectors, composition is vector addition
-- ie: Low :*: High = Mid
data Direction = Lef | Righ | Forward
               | Backward | Center
               | Low | Mid | High
               | Direction :*: Direction
     deriving (Show, Eq, Read)

data Angle = Angle Double
        deriving (Show, Eq, Read)

data Plane = XY | YZ | XZ
        deriving (Show, Eq, Read)

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
        deriving (Show, Eq, Read)

-- should we have an inherent "rhythm" like Tidal does?
-- I thinks yes
type Duration = Double

-- origins indexed by integers
data Origin = O Int
    deriving (Show, Eq, Read)

data Action = A Direction Length
    deriving (Show, Eq, Read)


instance Symmetric Action where
    refl pl (A dir len) = A (refl pl dir) len

-- super: "to which b is attached"
-- child: "parts attached to b"
-- this doesn't make sense to me
-- we really want a collection of parts to also be a part: contains/containedBy?
class Parts b where
    contains :: b -> [b]
    origin :: b -> Origin

-- binary tree with pointer to parent node
data KineChain a = Joint Origin (KineChains a)
                 | Link Origin a
     deriving (Show, Eq)

type KineChains a = [KineChain a]

-- can add more data constructors for different robot representations
-- ie: graphs, forests, fingertrees
type Robot a = KineChain a

instance Parts (KineChain a) where
    contains (Link o n) = [Link o n]
    contains (Joint o ks) = concatMap contains ks
    origin (Link o _) = o
    origin (Joint o _) = o

type Mult = Double

-- what if we made this a bimonoid typeclass instead?
data Dance b = Prim Action Mult b
             | Rest Mult
             | Skip -- id for series, parallel
             | Dance b :+: Dance b -- in series
             | Dance b :||: Dance b -- in parallel
        deriving (Show, Eq, Read)

-- map over parts (for changing platforms)
instance Functor Dance where
    fmap f (x :+: y) = (fmap f x) :+: (fmap f y)
    fmap f (x :||: y) = (fmap f x) :||: (fmap f y)
    fmap f (Rest m) = Rest m
    fmap f (Skip) = Skip
    fmap f (Prim act m part) = Prim act m (f part)

newtype ParDance a = ParDance { getPar :: Dance a }
    deriving (Eq, Show, Read, Functor)

instance (Parts a) => Monoid (ParDance a) where
    mempty = ParDance Skip
    ParDance x `mappend` ParDance y = ParDance (x :||: y)

newtype SeqDance a = SeqDance { getSeq :: Dance a }
    deriving (Eq, Show, Read, Functor)

instance (Parts a) => Monoid (SeqDance a) where
    mempty = SeqDance Skip
    SeqDance x `mappend` SeqDance y = SeqDance (x :+: y)

-- combinators
--------------

seqL, parL :: (Parts a) => [Dance a] -> Dance a
seqL ds =
    let m = fromIntegral $ length ds
        sped = map (changeTiming m) ds
    in getSeq . mconcat $ map SeqDance sped

parL = getPar . mconcat . map ParDance

changeTiming :: (Parts a) => Mult -> Dance a -> Dance a
changeTiming m (Prim a n b) = Prim a (m*n) b
changeTiming m (Rest n) = Rest (m*n)
changeTiming _ Skip = Skip
changeTiming m (d1 :+: d2) = (changeTiming m d1) :+: (changeTiming m d2)
changeTiming m (d1 :||: d2) = (changeTiming m d1) :||: (changeTiming m d2)

repeatn :: (Parts a) => Int -> Dance a -> Dance a
repeatn n dance = seqL $ take n $ repeat dance

-- transformers
---------------


-- map over all actions in a dance
transform :: (Parts a) => (Action -> Action) -> Dance a -> Dance a
transform f (x :+: y) = (transform f x) :+: (transform f y)
transform f (x :||: y) = (transform f x) :||: (transform f y)
transform f (Rest m) = Rest m
transform f (Prim act m dur) = Prim (f act) m dur


-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

