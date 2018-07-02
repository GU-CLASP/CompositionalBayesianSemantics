{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Lib where

import Numeric
import Prelude hiding (and,or,(>))
import Control.Monad.RWS
import System.Process
import System.Exit
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Statistics.Sample.KernelDensity
import Text.Read (readMaybe)

--------------------------------------------
-- Basic code-gen infrastructure

type a ⊸ b = a -> b
data Expr t where
  Expr :: String -> Expr t -- constant expression
  Lam :: (Expr a -> Expr t) -> Expr (a -> t) -- lambda
  App :: Expr (a -> t) -> Expr a -> Expr t -- application
  BinOp :: (String -> String -> String) -> Expr a -> Expr b -> Expr c
  UnOp :: (String -> String) -> Expr a -> Expr b
  If :: Prop -> Expr a -> Expr a -> Expr a
  -- Let :: P (Expr a) -> (Expr a ⊸ Expr b) -> Expr b
  Thunk :: P (Expr t) -> Expr t

class Representable a where
  constant :: a -> Expr a

instance Representable Bool where
  constant True = Expr "true"
  constant False = Expr "false"

instance Representable Float where
  constant = Expr . show

instance Fractional (Expr Float) where
  fromRational x = constant (fromRat x)
  (/) = BinOp (infixOp "/")

instance Num (Expr Float) where
  abs = UnOp (\x -> "Math.abs(" ++ x ++ ")")
  (-) = BinOp (infixOp "-")
  (+) = BinOp (infixOp "+")
  fromInteger x = constant (fromInteger x)
  (*) = BinOp (infixOp "*")

instance Floating (Expr Float) where
  sqrt = UnOp (\x -> "Math.sqrt(" ++ x ++ ")")
  exp = UnOp (\x -> "Math.exp(" ++ x ++ ")")
  log = UnOp (\x -> "Math.log(" ++ x ++ ")")

render :: Expr a -> P String
render = \case
  UnOp op x -> do
    x' <- render x
    return (op x')
  BinOp op x y -> do
    x' <- render x
    y' <- render y
    return (op x' y')
  Expr x -> return x
  App (Lam f) x -> render (f x)
  Lam f -> do
    x <- newVar
    body <- render (f (Expr x))
    return ("function( " ++ x ++ ") { return " ++ body ++ "; }")
  Thunk f -> do
    (x,body) <- censor (\_ -> []) $ listen f
    x' <- render x
    return ("function() {\n" ++ unlines body ++ " return " ++ x' ++ ";\n }")
  App x y -> do
    x' <- render x
    y' <- render y
    return ("(" ++ x' ++ ")(" ++ y' ++ ")")
  If cond x y -> do
    c' <- render cond
    x' <- render x
    y' <- render y
    return ("(" ++ c'  ++ "?" ++ x' ++ ":" ++ y' ++ ")")
  -- Let e f -> do
  --   x <- newVar
  --   e' <- render =<< e -- works only if f uses its argument at most once.
  --   fx <- render (f (Expr x))
  --   return ("var " ++ x ++ " = "++ e' ++";\n" ++ fx)

infixl #
(#) :: Expr (a -> b) -> Expr a -> Expr b
(#) = App

parens :: [Char] -> [Char]
parens x = "("++ x ++")"
infixOp :: [Char] -> [Char] -> [Char] -> [Char]
infixOp op x y = parens x ++ op ++ parens y
binFunc :: [Char] -> [Char] -> [Char] -> [Char]
binFunc f x y = f ++ "("++ x ++ "," ++ y ++")"

unFunc ::  [Char] -> [Char] -> [Char]
unFunc f x = f ++ "("++ x ++ ")"

newtype P a = P (RWS () [String] State a) deriving (Functor,Applicative,Monad,MonadState State, MonadWriter [String])

data State = State {nextVar :: Int}

logtrace :: String -> Expr a -> P ()
logtrace msg x = do
  x' <- render x
  emit ("console.log(" ++ show "LOG: " ++ "+" ++ show msg ++ "+ \" \" +" ++ x' ++ ");")

compileModel :: String -> P (Expr a) -> String
compileModel mainFunc m = unlines (z ++ [x])
  where (x,_,z) = runRWS p () (State {nextVar = 0})
        (P p) = render (UnOp (unFunc mainFunc) (Thunk m))


and :: Prop -> Prop -> Prop
and = BinOp (infixOp "&&")

(∧) :: Prop -> Prop -> Prop
(∧) = and

iff :: Prop -> Prop -> Prop
iff = BinOp (infixOp "==")

or :: Prop -> Prop -> Prop
or = BinOp (infixOp "||")

not' :: Prop -> Prop
not' = UnOp (\x -> "(!(" ++ x ++ "))")


(-->) :: Prop -> Prop -> Prop
p --> q = not' p `or` q

-- | Allocate a new variable
newVar :: P String
newVar = do
  n <- gets nextVar
  modify $ \State{..} -> State {nextVar = n+1, ..}
  return ("v" ++ show n)

emit :: String -> P ()
emit x = tell [x]

-----------------------------------
-- Types

type Vec = [Expr Float]
data Distrib a

type Ind = Vec
type Mat = [Vec]
-- type Vec = Expr Vector
type Prop = Expr Bool
type Pred = Ind -> Prop
type Measure = Ind -> Expr Float
type Adj = Vec
type AP = Measure
type CN = Ind -> Prop
type VP = Ind -> Prop
type NP = VP -> Prop
type Quant = CN -> NP

----------------------------------------------------
-- Compositional semantics


observe :: Prop -> P ()
observe = hyp

squared :: Num a => a -> a
squared x = x*x

observeEqual :: Expr Float -> Expr Float -> P ()
observeEqual x y = do
  f <- render (negate (squared (x-y)))
  emit ("factor(" ++ f ++ ");")

hyp :: Prop -> P ()
hyp x = do
  x' <- render x
  emit ("hyp(" ++ x' ++ ");")

-- | Sample new individual
newInd :: P Ind
newInd = newIndSuch []

-- | Sample new individual which satisfies some additional predicates
newIndSuch :: [Pred] -> P Ind
newIndSuch hs = do
  x <- newVector
  forM_ hs $ \h -> hyp (h x)
  return x

numberOfDimensions :: Int
numberOfDimensions = 2

newVector :: P Vec
newVector = mapM (uncurry sampleGaussian) (replicate numberOfDimensions (0,1))

newNormedVector :: P Vec
newNormedVector = do
  xs <- mapM (uncurry sampleGaussian) (replicate numberOfDimensions (0,1))
  return ((/ norm xs) <$> xs)

cosineDistance :: Vec -> Vec -> Expr Float
cosineDistance x y = dotProd x y / (norm x * norm y)

norm2 :: Vec -> Expr Float
norm2 x = dotProd x x

norm :: Vec -> Expr Float
norm = sqrt . norm2

newMatrix = mapM (\_ -> newVector) (replicate numberOfDimensions ())
newMatrix :: P Mat

satisfyAll :: [Ind] -> [Pred] -> P ()
satisfyAll xs ps = forM_ xs $ \x -> forM ps $ \p -> hyp (p x)

dotProd :: Vec -> Vec -> Expr Float
dotProd x y = sum (zipWith (*) x y)

vecMatProd :: Vec -> Mat -> Vec
vecMatProd v = map (dotProd v)

type Scalar = (Vec,Expr Float)

newScalarA :: P Scalar
newScalarA = do
  bias <- sampleGaussian 0 1
  v <- newNormedVector
  return (v,bias)

sampleGaussian :: Expr Float -> Expr Float -> P (Expr Float)
sampleGaussian mu sigma = do
  v <- newVar
  let m = Expr v
  mu' <- render mu
  sigma' <- render sigma
  emit ("var " ++ v ++ " = gaussian("  ++ mu' ++ "," ++ sigma' ++ ");")
  return m

newClass :: P Mat
newClass = newMatrix

forClass :: Mat -> Adj -> AP
forClass cl adj x = dotProd (vecMatProd adj cl) x

-- -- Alternative for scalar adjectives. We take the measure to be greater than that of a "random" element of the class.
-- forClassScalar :: Mat -> Adj -> AP
-- forClassScalar cl a x = Let (newIndSuch [isClass cl]) (\y -> adjAP a x - adjAP a y)

isClass :: Mat -> Pred
isClass clas x = dotProd (head clas) x > 0


adjAP :: Adj -> AP
adjAP = dotProd

gaussian :: Expr Float -> Expr Float -> Expr Float
gaussian mean stdDev = BinOp (binFunc "gaussian") mean stdDev

vague :: Float -> Measure -> Measure
vague vagueness m x = m x + gaussian 0 (Expr (show vagueness))

newMeasure :: P Measure
newMeasure = do
  (v,bias) <- newScalarA
  return (\x -> bias + dotProd v x)

positive :: Expr Float -> Prop
positive x = greaterThan x 0

newPred :: P (Ind -> Prop)
newPred = do
  m <- newMeasure
  return (\x -> positive (m x))

sample :: Expr (Distrib Bool) -> Expr Bool
sample d = Expr "sample" # d

bernouilli :: Expr Float -> Expr (Distrib Bool)
bernouilli = UnOp (\x -> "Bernoulli({p:" ++ x ++ "})")

-- TODO: there is a choice here.

-- this is an "intuitionistic" probably (it does not exclude the strong implication)
probablyInt :: Expr Float -> Prop -> Prop
probablyInt v x = sample (bernouilli v) --> x

-- this is a "definite" probably (it excludes the strong implication)
probablyDef ::  Expr Float -> Prop -> Prop
probablyDef v x = If (sample (bernouilli v)) x (not' x)

--  "a --> b"
-- and "if a then b else (not b)"
-- are not the same!

expectedPortion :: P Prop -> Expr Float
expectedPortion = UnOp (unFunc "expectedPortion") . Thunk

genQProb :: Expr Float -> Quant
genQProb prob cn vp = expectedPortion p > prob
  where p = do x <- newInd
               observe (cn x)
               return (vp x)

many :: Quant
many = genQProb 0.6

most :: Quant
most = genQProb 0.7

few :: Quant
few cn vp = genQProb 0.8 cn (not' . vp)

some :: Quant
some = genQProb 0.1

every :: Quant
every = genQProb 0.99

is :: Measure -> Pred
is m x =  (m x) > 0

more :: Measure -> Ind -> Ind -> Prop
more m x y = (m x) > (m y)

greaterThan :: Expr Float -> Expr Float -> Prop
greaterThan = BinOp (infixOp ">")

(>) :: Expr Float -> Expr Float -> Prop
(>) = greaterThan

-- equal :: Expr Float -> Expr Float -> Prop
-- equal x y = 0.1 > (abs (x - y))


equal :: Expr Float -> Expr Float -> Prop
equal x y = 0.1 > (abs (x - y))


disjoint :: Pred -> Pred -> Pred
disjoint p q x = not' (p x) `or` not' (q x)


subsective :: Adj -> Mat -> Pred
subsective a cl x = isClass cl x `and` is (forClass cl a) x

-- An alternative semantics for subsective scalars.

anything :: Pred
anything _ = Expr "true"

plot :: String -> [Double] -> IO ()
plot prefix xs = do
  let (xs',ys') = kde 64 (V.fromList xs)
      fname = prefix ++ ".svg"
  let ls = 
        ["set terminal svg size 350,262 fname 'Verdana' enhanced background rgb 'white'",
         "set output '" ++ fname ++ "'",
         "set key off", -- no legend
          "$data << EOD"] ++
        [show x ++ " " ++ show y | (x,y) <- zip (V.toList xs') (V.toList ys')] ++
        ["EOD", "plot '$data' with lines"] -- filledcurve
  putStrLn "Plotting results..."
  (code,output,errors) <- readProcessWithExitCode "gnuplot" ["-p"] (unlines ls)
  case code of
    ExitFailure _ -> do
      putStrLn "Gnuplot failed with input:"
      putStrLn (unlines ls)
      putStrLn "errors:"
      putStrLn output
      putStrLn errors
    ExitSuccess ->
      putStrLn ("Plot output to " ++ fname)
  return ()
  
class KnownTyp a where
  isContinuous :: Bool

instance KnownTyp Float where
  isContinuous = True

instance KnownTyp Bool where
  isContinuous = False

parseValues :: [String] -> IO [Double]
parseValues [] = return []
parseValues (v:vs) = case readMaybe v of
  Nothing -> putStrLn v >> parseValues vs
  Just x -> (x:) <$> parseValues vs

run :: forall a. KnownTyp a => P (Expr a) -> IO ()
run model = do
  putStrLn "Creating model..."
  rts <- readFile "../Frontend/RTS.wppl"
  let m = compileModel mainName model
      funname = "modelFunction"
      mainName = if isContinuous @a then "mainContinuous" else "mainDiscrete"
      fname = funname ++ ".wppl"
  writeFile fname (intercalate "\n\n" [rts,m])
  putStrLn "Running webppl..."
  (code,output,errors) <- readProcessWithExitCode "webppl" [fname] ""
  case code of
    ExitFailure _ -> do
      putStrLn "Webppl failed with errors:"
      putStrLn output
      putStrLn errors
    ExitSuccess -> do
      putStrLn "Success!"
      case isContinuous @a of
        True -> do
            values <- parseValues (lines output)
            plot funname values
        False -> putStrLn output

