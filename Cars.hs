import Lib

exampleCars :: P (Expr Bool)
exampleCars = do
  red <- newPred
  notRed <- newPred
  car <- newPred
  fast <- newPred

  hypUniv (disjoint red notRed)
  hypUniv (most car red)
  hypUniv (most car fast)

  x <- newInd
  hyp (car x)
  return (fast x)

main = run exampleCars
