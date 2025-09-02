{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataTypeALaCarte where

import Control.Applicative (Alternative ((<|>)))

-- * 1 Introduction

-- data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int
-- eval (Val x) = x
-- eval (Add x y) = eval x + eval y

-- render :: Expr -> String
-- render (Val x) = show x
-- render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

-- * 2 Fixing the expression problem

data Expr f = In (f (Expr f))

data Val e = Val Int

type IntExpr = Expr Val

data Add e = Add e e

type AddExpr = Expr Add

data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

-- * 3 Evaluation

instance Functor Val where
  fmap :: (a -> b) -> Val a -> Val b
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap :: (a -> b) -> Add a -> Add b
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap :: (a -> b) -> (f :+: g) a -> (f :+: g) b
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: (Functor f) => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class (Functor f) => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra :: Val Int -> Int
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra :: Add Int -> Int
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra :: (f :+: g) Int -> Int
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: (Eval f) => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- >>> eval addExample
-- 1337

-- * 4 Automating injections

-- val :: Int -> Expr Val
-- val x = In (Val x)

-- | Original ⊕ (\oplus)
infixl 6 |+|

-- (|+|) :: Expr Add -> Expr Add -> Expr Add
-- x |+| y = In (Add x y)

-- val 1 |+| val 3 -- type error

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

  -- | ch 5
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj :: f a -> f a
  inj = id

  -- \| ch 5
  prj :: f a -> Maybe (f a)
  prj = Just . id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj :: f a -> (f :+: g) a
  inj = Inl

  -- \| ch 5
  prj :: (f :+: g) a -> Maybe (f a)
  prj (Inl fx) = Just $ fx
  prj (Inr gx) = Nothing

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj :: f a -> (h :+: g) a
  inj = Inr . inj

  -- \| ch 5
  prj :: (h :+: g) a -> Maybe (f a)
  prj (Inl fx) = Nothing
  prj (Inr gx) = prj gx

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

(|+|) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x |+| y = inject (Add x y)

-- >>> eval ex41
-- 31337
ex41 :: Expr (Add :+: Val)
ex41 = val 30000 |+| val 1330 |+| val 7

inVal :: Int -> Expr (Val :+: Val)
inVal i = inject (Val i)

-- >>> check4
-- True
check4 :: Bool
check4 = eval exA == eval exB
  where
    exA :: Expr (Val :+: Val)
    exA = In (Inl (Val 1))

    exB :: Expr (Val :+: Val)
    exB = In (Inr (Val 1))

-- ex0 :: Expr (Add :+: Val)
-- ex0 = val 10 |+| val 20

ex0' :: Expr (Add :+: Val)
ex0' = In (Inl (Add (In (Inr (Val 10))) (In (Inr (Val 20)))))

-- * 5 Example

data Mul x = Mul x x

instance Functor Mul where
  fmap :: (a -> b) -> Mul a -> Mul b
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra :: Mul Int -> Int
  evalAlgebra (Mul x y) = x * y

-- | Original ⊗ (\otimes)
infixl 7 |*|

(|*|) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x |*| y = inject (Mul x y)

infixr 6 :+:

-- >>> eval ex51
-- 404
ex51 :: Expr (Val :+: Add :+: Mul)
ex51 = val 80 |*| val 5 |+| val 4

-- |
-- >>> eval ex51'
-- 404
--
-- If "infixr 6 :+:" is not specified, parentheses are required for the expression type.
ex51' :: Expr (Val :+: (Add :+: Mul))
ex51' = val 80 |*| val 5 |+| val 4

-- >>> eval ex52
-- 42
ex52 :: Expr (Val :+: Mul)
ex52 = val 6 |*| val 7

-- class Render f where
--   render :: f (Expr f) -> String
class Render f where
  render :: (Render g) => f (Expr g) -> String

pretty :: (Render f) => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render :: (Render g) => Val (Expr g) -> String
  render (Val i) = show i

instance Render Add where
  render :: (Render g) => Add (Expr g) -> String
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render :: (Render g) => Mul (Expr g) -> String
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render :: (Render h) => (f :+: g) (Expr h) -> String
  render (Inl x) = render x
  render (Inr y) = render y

-- >>> pretty ex53
-- "((80 * 5) + 4)"
ex53 :: Expr (Val :+: Add :+: Mul)
ex53 = val 80 |*| val 5 |+| val 4

-- class (Functor sub, Functor sup) => sub :<: sup where
--   inj :: sub a -> sup a
--   prj :: sup a -> Maybe (sub a)

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

-- | Origina name `distr`
distrR :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distrR t = do
  Mul a b <- match t
  Add c d <- match b
  return (a |*| c |+| a |*| d)

distrL :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distrL t = do
  Mul a b <- match t
  Add c d <- match a
  return (c |*| b |+| d |*| b)

distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = distrR t <|> distrL t

prettyDistr :: (Add :<: f, Mul :<: f, Render f) => Expr f -> String
prettyDistr expr = maybe "error" pretty $ distr expr

-- >>> pretty ex54
-- "(10 * (20 + 30))"
-- >>> prettyDistr ex54
-- "((10 * 20) + (10 * 30))"
ex54 :: Expr (Val :+: Mul :+: Add)
ex54 = val 10 |*| (val 20 |+| val 30)

-- >>> pretty ex55
-- "((20 + 30) * 10)"
-- >>> prettyDistr ex55
-- "((20 * 10) + (30 * 10))"
ex55 :: Expr (Val :+: Mul :+: Add)
ex55 = (val 20 |+| val 30) |*| val 10

ex56 :: Expr (Val :+: Mul :+: Add)
ex56 = (val 10 |+| val 20) |*| (val 30 |+| val 40)
