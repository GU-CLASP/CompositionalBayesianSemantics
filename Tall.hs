import Lib

modelTall1 :: P (Expr Bool)
modelTall1 = do
  tall <- vague 3 <$> newMeasure
  john <- newInd
  mary <- newInd
  hyp (more tall john mary)

  return (is tall john)

modelTallV :: P Prop
modelTallV = do
  tall <- vague 3 <$> newMeasure
  john <- newInd
  mary <- newInd
  hyp (more tall john mary)

  return (is tall john)


modelTallContr :: P Prop
modelTallContr = do
  tall <- vague 3 <$> newMeasure
  john <- newInd
  mary <- newInd

  return (more tall john mary ∧ more tall mary john)

main :: IO ()
main = run modelTallContr
