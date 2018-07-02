module GenQuantExamples where

import Lib
import Control.Monad
import Prelude hiding (or,and)

ex1 :: P (Expr Bool)
ex1 = do
  car <- newPred
  red <- newPred

  observe (most car red)
  return (some car red)

-- >>> run ex1
-- Marginal:
--     true : 0.999
--     false : 0.001


ex2 :: P (Expr Bool)
ex2 = do
  car <- newPred
  red <- newPred

  observe (every car red)

  return (most car red)

-- >>> run ex2
-- Marginal:
--     true : 0.991
--     false : 0.009


ex3a :: P (Expr Bool)
ex3a = do
  chair <- newPred
  fourlegs <- newPred

  observe (many chair fourlegs)

  x <- newIndSuch [fourlegs]
  return (chair x)


-- >>> run ex3a
-- Marginal:
--     true : 0.653
--     false : 0.347

ex3b :: P (Expr Bool)
ex3b = do
  chair <- newPred
  fourlegs <- newPred

  observe (many chair fourlegs)

  x <- newIndSuch [chair]
  return (fourlegs x)


-- >>> run ex3b
-- Marginal:
--     true : 0.821
--     false : 0.179




ex3c :: P (Expr Bool)
ex3c = do
  chair <- newPred
  fourlegs <- newPred
  comfy <- newPred

  observe (most chair fourlegs)
  observe (most chair comfy)

  x <- newIndSuch [chair]
  return (fourlegs x)

-- >>> run ex3c
-- Success!
-- Marginal:
--     true : 0.925
--     false : 0.075

ex3d :: P (Expr Bool)
ex3d = do
  chair <- newPred
  fourlegs <- newPred

  observe (many chair fourlegs)
  observe (most anything (not' . chair))

  x <- newIndSuch [fourlegs]
  return (chair x)


-- Marginal:
--     false : 0.779
--     true : 0.221


ex4a :: P (Expr Bool)
ex4a = do
  animal <- newPred
  bird <- newPred
  fly <- newPred

  observe (most bird fly)
  observe (some animal bird)

  x <- newIndSuch [animal]
  return (fly x)

-- >>> run ex4a
-- Creating model...
-- Running webppl...
-- Success!
-- Marginal:
--     true : 0.778
--     false : 0.222

ex4c :: P (Expr Bool)
ex4c = do
  fly <- newPred
  observe (most anything (\x -> not' (fly x)))

  bird <- newPred
  observe (most anything (\x -> not' (bird x)))
  observe (most bird fly)

  x <- newIndSuch [bird]
  return (fly x)


-- >>> run ex4c
-- Success!
-- Marginal:
--     true : 0.78
--     false : 0.22

ex4d :: P (Expr Bool)
ex4d = do
  animal <- newPred
  bird <- newPred
  fly <- newPred

  observe (most animal (\x -> not' (fly x)))
  observe (most bird fly)
  observe (most animal (not' . bird))
  observe (every bird animal)

  x <- newIndSuch [bird]
  return (fly x)

-- Marginal:
--     true : 0.9
--     false : 0.1


ex4e :: P (Expr Bool)
ex4e = do
  animal <- newPred
  bird <- newPred
  fly <- newPred

  observe (most animal (\x -> not' (fly x)))
  observe (most bird fly)
  observe (most animal (not' . bird))
  observe (every bird animal)

  x <- newIndSuch [animal]
  return (fly x)


-- Marginal:
--     false : 0.807
--     true : 0.193

ex4f :: P (Expr Bool)
ex4f = do
  animal <- newPred
  bird <- newPred
  fly <- newPred

  observe (most animal (\x -> not' (fly x)))
  observe (most bird fly)
  observe (every bird animal)

  x <- newIndSuch [animal]
  return (bird x)


-- Marginal:
-- false : 0.743
-- true : 0.257

ex4g :: P (Expr Bool)
ex4g = do
  animal <- newPred
  bird <- newPred
  fly <- newPred

  observe (most animal (not' . fly))
  observe (most bird fly)
  observe (every bird animal)
  return (most animal (not' . bird))


main :: IO ()
main = run ex4g
