{-# OPTIONS_GHC -F -pgmF she#-}

{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , RankNTypes
  , TypeOperators
  , TypeSynonymInstances
  , FlexibleInstances
  , LambdaCase
  , TypeInType
  , ScopedTypeVariables
  , TypeFamilies
#-}

{-----------------------------------------------------------------------------
















  BITE ME!

  In Search Of The Rational Fixpoint (Of Normal Functors)

  Conor Mc Bride
























-----------------------------------------------------------------------------}

module Rat101 where

import Data.Traversable
import Data.Void
import Debug.Trace

parp :: x -> x
parp = trace "PARP!"

{----------------------------------------------------------------------------}

-- PREAMBLE

unfo :: (s -> Maybe (x, s)) -> s -> [x]
unfo coalg seed = case coalg seed of
  Nothing        -> []
  Just (x, seed) -> parp $ x : unfo coalg seed

type NEL x = (x, [x])

one :: x -> NEL x
one x = (x, [])

more :: x -> NEL x -> NEL x
more x (y, ys) = (x, y : ys)

cyc0 :: NEL x -> [x]
cyc0 = unfo rotate where
  rotate (x, xs) = Just (x, foldr more (one x) xs)

mycat :: [x] -> [x] -> [x]
mycat []       ys = ys
mycat (x : xs) ys = parp $ x : mycat xs ys

cyc1 :: NEL x -> [x]
cyc1 (x, xs) = ys where ys = mycat (x : xs) ys

-- can we do this in general?






























{----------------------------------------------------------------------------}

-- NATURAL NUMBERS (we'll need them in types)

data N = Z | S N

{-(-}
toInt :: N -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n
{-)-}

{-(-}
instance Show N where
  show = show . toInt

instance Num N where
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n - 1))
  Z + y = y
  S x + y = S (x + y)
  Z * y = Z
  S x * y = y + x * y
  abs n = n
  signum Z = Z
  signum _ = S Z
  negate _ = Z
{-)-}


















{------------------------------------------------------------------------}

-- NORMAL FUNCTORS

{-(-}
data Normal (f :: N -> *)(x :: *) = forall n. f n :- Vec n x
infix 5 :-

data Vec (n :: N)(x :: *) where
  VN   :: Vec Z x
  (:&) :: x -> Vec n x -> Vec (S n) x
infixr 6 :&
{-)-}

{-(-}
instance Traversable (Normal f) where
  traverse f (c :- ss) = (| (|c|) :- traverse f ss |)

instance Traversable (Vec n) where
  traverse f VN        = (| VN |)
  traverse f (s :& ss) = (| f s :& traverse f ss |)

instance Show x => Show (Vec n x) where
  show = show . foldMap (:[])
{-)-}


















{------------------------------------------------------------------------}

-- TREES (FREE MONADS) OF NORMAL FUNCTORS

{-(-}
data Tree (f :: N -> *) (x :: *)
  = Le x
  | No (Normal f (Tree f x))
{-)-}

{-(-}
type Mu f = Tree f Void

data Listig (x :: *)(n :: N) where
  NI :: Listig x Z
  CO :: x -> Listig x (S Z)
{-)-}

{-(-}
type MyList x = Mu (Listig x)
myNil :: MyList x
myNil = No (NI :- VN)
myCons :: x -> MyList x -> MyList x
myCons x xs = No (CO x :- xs :& VN)

myList :: Mu (Listig N)
myList = myCons 0 $ myCons 1 $ myCons 2 $ myNil
{-)-}

{-(-}
class ShowN (f :: N -> *) where
  showN :: f n -> String

instance (ShowN f, Show x) => Show (Tree f x) where
  show (Le x) = show x
  show (No (c :- xs)) = showN c ++ "-" ++ show xs

instance Show x => ShowN (Listig x) where
  showN NI = "*"
  showN (CO x) = show x
{-)-}











{--------------------------------------------------------------------------}

-- FOLD AND UNFOLD

{-(-}
foMu :: (Normal f t -> t) -> Mu f -> t
foMu alg (No (c :- xs)) = alg (c :- fmap (foMu alg) xs)
{-)-}

{-(-}
type Nu f = Tree f Void

unfoNu :: (s -> Normal f s) -> s -> Nu f
unfoNu coalg s = case coalg s of
  c :- ss -> No (c :- fmap (unfoNu coalg) ss)
{-)-}



















{--------------------------------------------------------------------------}

-- SPANNING TREES AND PATHS THEREIN

{-(-}
data One = Stub deriving Show

type Spanner f = Tree f One  -- finite, morally
{-)-}


{-(-}
data Path (x :: Spanner f) where
  Stop ::                        Path (No (c :- xs))
  Step :: VElt x xs -> Path x -> Path (No (c :- xs))

data VElt (a :: x)(as :: Vec n x) where
  Head :: VElt a (a :& as)
  Tail :: VElt a as -> VElt a (b :& as)
{-)-}


























{--------------------------------------------------------------------------}

-- LIFTING

{-(-}
data Veccy (xy :: x -> *) (xs :: Vec n x) where
  VNy   :: Veccy xy VN
  (:&^) :: xy x -> Veccy xy xs -> Veccy xy (x :& xs)

projecty :: Veccy xy xs -> VElt x xs -> xy x
projecty (y :&^ _)  Head     = y
projecty (_ :&^ ys) (Tail i) = projecty ys i
{-)-}





























{--------------------------------------------------------------------------}

-- ORNAMENTING SPANNING TREES

{-(-}
data Deco (le :: *) (no :: *) (t :: Spanner f) where
  Leafy :: le -> Deco le no (Le Stub)
  Nodey :: no -> f n -> Veccy (Deco le no) xs
        -> Deco le no (No ((c :: f n) :- xs))

noDeco :: Deco le no t -> Path t -> no
noDeco (Nodey no _ _) Stop = no
noDeco (Nodey _ _ ns) (Step i p) = noDeco (projecty ns i) p
{-)-}














{--------------------------------------------------------------------------}

-- BIRD SONG

{-(-}
type Rho f = Tree f Void

data Wiring (f :: N -> *) where
  Wired :: Deco (Path t) One (t :: Spanner f) -> Wiring f

richard :: Wiring f -> Rho f
richard (Wired d) = r where (m, r) = bird m d

bird :: forall f t s.
        Deco (Path t) (Rho f) (t :: Spanner f) 
     -> Deco (Path t) One (s :: Spanner f)
     -> (Deco (Path t) (Rho f) (s :: Spanner f), Rho f)
     
birds :: forall f t n ss.
         Deco (Path t) (Rho f) (t :: Spanner f) 
      -> Veccy (Deco (Path t) One) (ss :: Vec n (Spanner f))
      -> (Veccy (Deco (Path t) (Rho f)) ss, Vec n (Rho f))
      
bird m (Leafy p) = (Leafy p, noDeco m p)
bird m (Nodey Stub c xs) = (Nodey r c ds, r) where
  r        = parp $ No (c :- rs)
  (ds, rs) = birds m xs
  
birds m VNy = (VNy, VN)
birds m (x :&^ xs) = (d :&^ ds, r :& rs) where
  (d,  r)  = bird m x
  (ds, rs) = birds m xs
{-)-}































{--------------------------------------------------------------------------}

-- ISN'T THIS WHERE WE CAME IN?


{-(-}
chop :: Int -> Rho f -> Spanner f
chop 0 _ = Le Stub
chop n (No (c :- ts)) = No (c :- fmap (chop (n - 1)) ts)

data Pre (f :: N -> *) where
  MkPre :: Deco One One (x :: Spanner f) -> Pre f

cyRho :: forall x. Mu (Listig x) -> Rho (Listig x)
cyRho (No (NI :- VN)) = richard $ Wired $ Nodey Stub NI VNy
cyRho (No (CO x :- xs :& VN)) = richard $ case scramble xs of
    MkPre p -> Wired $ Nodey Stub (CO x) (squinch p :&^ VNy)
  where
  
  squinch :: forall (z :: Spanner (Listig x)) y (c :: Listig x (S Z)) a.
    Deco One One z -> Deco (Path (No (c :- a :& VN))) One z
  squinch (Leafy Stub) = Leafy Stop
  squinch (Nodey Stub c (xs :&^ VNy)) = Nodey Stub c (squinch xs :&^ VNy)
  
  scramble :: Mu (Listig x) -> Pre (Listig x)
  scramble (No (NI :- VN)) = MkPre (Leafy Stub)
  scramble (No (CO x :- xs :& VN)) = case scramble xs of
    MkPre d -> MkPre (Nodey Stub (CO x) (d :&^ VNy))
{-)-}



