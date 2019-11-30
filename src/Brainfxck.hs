module Brainfxck
  ( Brainfxck
  , forward
  , backward
  , increment
  , decrement
  , out
  , substitution
  , loop
  , runBrainfxck
  ) where

import Control.Lens ((%~), _1, _2)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Char (chr, ord)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

data Brainfxck r
  = Forward r
  | Backward r
  | Increment r
  | Decrement r
  | Out r
  | Substitution r
  | Loop (Free Brainfxck ()) r

instance Functor Brainfxck where
  fmap f (Forward r) = Forward $ f r
  fmap f (Backward r) = Backward $ f r
  fmap f (Increment r) = Increment $ f r
  fmap f (Decrement r) = Decrement $ f r
  fmap f (Out r) = Out $ f r
  fmap f (Substitution r) = Substitution $ f r
  fmap f (Loop b r) = Loop b $ f r

forward :: Free Brainfxck ()
forward = liftF $ Forward ()

backward :: Free Brainfxck ()
backward = liftF $ Backward ()

increment :: Free Brainfxck ()
increment = liftF $ Increment ()

decrement :: Free Brainfxck ()
decrement = liftF $ Decrement ()

out :: Free Brainfxck ()
out = liftF $ Out ()

substitution :: Free Brainfxck ()
substitution = liftF $ Substitution ()

loop :: Free Brainfxck () -> Free Brainfxck ()
loop process = liftF $ Loop process ()

type Env = (Memory, Pointer)

type Memory = Vector Int

type Pointer = Int

runBrainfxck :: Free Brainfxck () -> IO ()
runBrainfxck f = evalStateT (runBrainfxck' f) (V.replicate 1024 0, 0)

runBrainfxck' :: Free Brainfxck () -> StateT Env IO ()
runBrainfxck' = foldFree goBrainfxck

goBrainfxck :: Brainfxck r -> StateT Env IO r
goBrainfxck (Forward next) = modify (_2 %~ (+ 1)) >> return next
goBrainfxck (Backward next) = modify (_2 %~ subtract 1) >> return next
goBrainfxck (Increment next) = do
  (memory, pointer) <- get
  modify $ _1 %~ applyMemoryPointed (+ 1) pointer
  return next
goBrainfxck (Decrement next) = do
  (memory, pointer) <- get
  modify $ _1 %~ applyMemoryPointed (subtract 1) pointer
  return next
goBrainfxck (Out next) = do
  (memory, pointer) <- get
  lift . putChar . chr $ memory ! pointer
  return next
goBrainfxck (Substitution next) = do
  (memory, pointer) <- get
  c <- lift getChar
  modify $ _1 %~ applyMemoryPointed (const (ord c)) pointer
  return next
goBrainfxck (Loop loop next) = do
  runBrainfxck' loop
  (memory, pointer) <- get
  if (memory ! pointer) > 0
    then goBrainfxck (Loop loop next)
    else return next

applyMemoryPointed :: (Int -> Int) -> Pointer -> Memory -> Memory
applyMemoryPointed f pointer memory =
  memory // [(pointer, f (memory ! pointer))]
