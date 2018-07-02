import Lib

modelSimplest :: P (Expr Bool)
modelSimplest = do
  red <- newPred
  x <- newInd
  observe (red x)
  return (red x)

main :: IO ()
main = run modelSimplest
