{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataTypeALaCarte where

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

-- Original ⊕
infixl 6 |+|

-- (|+|) :: Expr Add -> Expr Add -> Expr Add
-- x |+| y = In (Add x y)

-- val 1 |+| val 3 -- type error

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance (Functor f) => f :<: f where
  inj :: f a -> f a
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj :: f a -> (f :+: g) a
  inj = Inl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj :: f a -> (h :+: g) a
  inj = Inr . inj

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

-- | original ⊗
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
