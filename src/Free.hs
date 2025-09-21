{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Free where

import DataTypeALaCarte ((:+:) (..), (:<:) (..))
import Prelude hiding (getChaar, putChar, readFile, writeFile)
import qualified Prelude

-- * 6 Monads for free

data Term f a
  = Pure a
  | Impure (f (Term f a))

instance (Functor f) => Functor (Term f) where
  fmap :: (a -> b) -> Term f a -> Term f b
  fmap f (Pure x) = Pure $ f x
  fmap f (Impure t) = Impure $ fmap (fmap f) t

instance (Functor f) => Applicative (Term f) where
  pure :: a -> Term f a
  pure = Pure

  (<*>) :: Term f (a -> b) -> Term f a -> Term f b
  Pure f <*> x = fmap f x
  Impure f <*> t = Impure $ fmap (<*> t) f

instance (Functor f) => Monad (Term f) where
  (>>=) :: (Functor f) => Term f a -> (a -> Term f b) -> Term f b
  Pure x >>= f = f x
  Impure t >>= f = Impure $ fmap (>>= f) t

data Zero a

data One a = One

data Const e a = Const e

type Tidentity a = Term Zero a

tIdentity :: a -> Tidentity a
tIdentity x = Pure x

type Tmaybe a = Term One a

tJust :: a -> Tmaybe a
tJust x = Pure x

tNothing :: Tmaybe a
tNothing = Impure One

type Teither e a = Term (Const e) a

tLeft :: e -> Teither e a
tLeft x = Impure (Const x)

tRight :: a -> Teither e a
tRight x = Pure x

-- | the integer with which to increment the memory, and the rest of the computation
data Incr t = Incr Int t

-- | functional argument that expects to receive the contents of the memory cell
data Recall t = Recall (Int -> t)

data Clear t = Clear t

{-
data Incr' t = Incr' Int (() -> t)
data Recall' t = Recall () (Int -> t)
data Clear' t = Clear' () (() -> t)
-}

instance Functor Incr where
  fmap :: (a -> b) -> Incr a -> Incr b
  fmap f (Incr i r) = Incr i (f r)

instance Functor Recall where
  fmap :: (a -> b) -> Recall a -> Recall b
  fmap f (Recall r) = Recall $ f . r

instance Functor Clear where
  fmap :: (a -> b) -> Clear a -> Clear b
  fmap f (Clear r) = Clear $ f r

inject :: (g :<: f) => g (Term f a) -> Term f a
inject = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject $ Incr i (Pure ())

recall :: (Recall :<: f) => Term f Int
recall = inject $ Recall Pure

clear :: (Clear :<: f) => Term f ()
clear = inject $ Clear (Pure ())

-- | another type @tick :: (Recall :<: f, Incr :<: f ) => Term f Int@
--
-- >>> run tick (Mem 4)
-- (4,Mem 5)
tick :: Term (Recall :+: Incr) Int
tick = do
  y <- recall
  incr 1
  return y

foldTerm :: (Functor f) => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure _ (Pure x) = pure x
foldTerm pure imp (Impure t) = imp $ fmap (foldTerm pure imp) t

newtype Mem = Mem Int
  deriving (Show)

class (Functor f) => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra :: Incr (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra :: Recall (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance Run Clear where
  runAlgebra :: Clear (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
  runAlgebra (Clear r) (Mem _) = r (Mem 0)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra :: (Run f, Run g) => (f :+: g) (Mem -> (a, Mem)) -> Mem -> (a, Mem)
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: (Run f) => Term f a -> (Mem -> (a, Mem))
run = foldTerm (,) runAlgebra

-- * 7 Applications

data Teletype a
  = GetChar (Char -> a)
  | PutChar Char a

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a

{-
data Teletype' a
  = GetChar' () (Char -> a)
  | PutChar' Char (() -> a)

data FileSystem' a
  = ReadFile' FilePath () (String -> a)
  | WriteFile' FilePath String (() -> a)
-}

instance Functor Teletype where
  fmap :: (a -> b) -> Teletype a -> Teletype b
  fmap f (GetChar g) = GetChar $ f . g
  fmap f (PutChar c x) = PutChar c (f x)

instance Functor FileSystem where
  fmap :: (a -> b) -> FileSystem a -> FileSystem b
  fmap f (ReadFile fp g) = ReadFile fp (f . g)
  fmap f (WriteFile fp str x) = WriteFile fp str (f x)

getChar :: (Teletype :<: f) => Term f Char
getChar = inject $ GetChar Pure

putChar :: (Teletype :<: f) => Char -> Term f ()
putChar c = inject $ PutChar c (Pure ())

readFile :: (FileSystem :<: f) => FilePath -> Term f String
readFile fp = inject $ ReadFile fp Pure

writeFile :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFile fp str = inject $ WriteFile fp str (Pure ())

exec :: (Exec f) => Term f a -> IO a
exec = foldTerm return execAlgebra

class (Functor f) => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra :: Teletype (IO a) -> IO a
  execAlgebra (GetChar f) = Prelude.getChar >>= f
  execAlgebra (PutChar c io) = Prelude.putChar c >> io

instance Exec FileSystem where
  execAlgebra :: FileSystem (IO a) -> IO a
  execAlgebra (ReadFile fp f) = Prelude.readFile fp >>= f
  execAlgebra (WriteFile fp str io) = Prelude.writeFile fp str >> io

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra :: (f :+: g) (IO a) -> IO a
  execAlgebra (Inl io) = execAlgebra io
  execAlgebra (Inr io) = execAlgebra io

-- |
-- >>> exec $ cat "./src/test.txt"
-- foo bar baz
-- foobar
cat :: FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
  contents <- readFile fp
  mapM putChar contents
  return ()
