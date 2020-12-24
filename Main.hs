{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- Example of pricing using the Binomial theorem
module Main where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data BinTree a 
  = Leaf a 
  | Node {
      value :: a,
      up :: BinTree a,
      down :: BinTree a
    } deriving Show

makeBaseFunctor ''BinTree

data Claim = Call | Put

main = do
  let s = 100.0
      σ = 0.2
      r = 0.05
      k = 100.0
      claim = Call
      t_T = 1.0
      american = False

      δt = 0.25
      u = 1.1
      v = 0.9
      p' = 0.5 + r * sqrt(δt) / 2.0 / σ
      disc = 1.0 / (1.0 + r * δt)
  putStrLn "Pricing ... "
  let π = ana g (100.0, 0.0) :: BinTree Double
          where g :: (Double, Double) -> Base (BinTree Double) (Double, Double)
                g (s, t) | t > t_T = error "δt must divide equally into T"
                g (s, t) | t < t_T = NodeF s (u * s, t + δt) (v * s, t + δt)
                g (s, t) | t == t_T = LeafF s
  putStrLn . show $ π
  -- putStrLn $ "Price is " <> (show . iter value $ π)
