import Lib

model :: P (Expr Bool)
model = do
  weight <- newMeasure
  -- footSize <- newMeasure
  -- height <- newMeasure
  isMale <- is <$> newMeasure

  let sampleWith :: Bool -> Float -> Float -> Float -> P Ind
      sampleWith male h w fs = do
        s <- newInd
        observe (isMale s `iff` constant male)
        -- hyp (height s `equal` constant h)
        observeEqual (weight s) (constant w)
        -- hyp (footSize s `equal` constant fs)
        return s

  _ <- sampleWith True  6.00 1.80 12
  _ <- sampleWith True  5.92 1.90 11
  _ <- sampleWith True  5.58 1.70 12
  _ <- sampleWith True  5.92 1.65 10
  _ <- sampleWith False 5.00 1.00  6
  _ <- sampleWith False 5.50 1.50  8
  _ <- sampleWith False 5.42 1.30  7
  _ <- sampleWith False 5.75 1.50  9

  x <- newInd

  observeEqual (weight x) 1.9
  return (isMale x)


modelCD :: P (Expr Float)
modelCD = do
  (vW,bW) <- newScalarA
  let weight = (\x -> bW + dotProd vW x)
   
  (vM,bM) <- newScalarA
  let male = (\x -> bM + dotProd vM x)
      isMale :: Ind -> Prop
      isMale = is male


  let sampleWith :: Bool -> Float -> Float -> Float -> P Ind
      sampleWith male h w fs = do
        s <- newInd
        observe (isMale s `iff` constant male)
        -- hyp (height s `equal` constant h)
        observeEqual (weight s) (constant w)
        -- hyp (footSize s `equal` constant fs)
        return s

  _ <- sampleWith True  6.00 1.80 12
  _ <- sampleWith True  5.92 1.90 11
  _ <- sampleWith True  5.58 1.70 12
  _ <- sampleWith True  5.92 1.65 10
  _ <- sampleWith False 5.00 1.00  6
  _ <- sampleWith False 5.50 1.50  8
  _ <- sampleWith False 5.42 1.30  7
  _ <- sampleWith False 5.75 1.50  9

  return (cosineDistance vM vW)


main :: IO ()
main = run model
