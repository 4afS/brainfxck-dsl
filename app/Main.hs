module Main where

import Brainfxck
  ( Brainfxck
  , backward
  , decrement
  , forward
  , increment
  , loop
  , out
  , runBrainfxck
  , substitution
  )
import Control.Monad.Free (Free)

main :: IO ()
main = runBrainfxck hello

hello :: Free Brainfxck ()
hello = do
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  loop $ do
    forward
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    forward
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    increment
    forward
    increment
    increment
    increment
    forward
    increment
    backward
    backward
    backward
    backward
    decrement
  forward
  out
  forward
  increment
  increment
  out
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  out
  out
  increment
  increment
  increment
  out
  forward
  increment
  increment
  increment
  increment
  increment
  out
  backward
  backward
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  increment
  out
  forward
  out
  increment
  increment
  increment
  out
  decrement
  decrement
  decrement
  decrement
  decrement
  decrement
  out
  decrement
  decrement
  decrement
  decrement
  decrement
  decrement
  decrement
  decrement
  out
  forward
  increment
  out
  forward
  increment
  out
