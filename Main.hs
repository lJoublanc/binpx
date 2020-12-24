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

data Tree a 
  = Leaf a 
  | Branch {
      value :: a,
      up :: Tree a,
      down :: Tree a
    } deriving Show

makeBaseFunctor ''Tree

data Claim = Call | Put

data Seed = Seed {
    s :: Double,
    t :: Double
  }

main = do
  let s = 100.0
      σ = 0.2
      r = 0.05
      k = 100.0
      claim = Call
      t_T = 1.0
      american = False

      δt = 0.25 -- consider providing n = T/δt instead
      u = 1.1
      v = 0.9
      p' = 0.5 + r * sqrt(δt) / 2.0 / σ
      disc = 1.0 / (1.0 + r * δt)
      payoff s Call = max 0.0 $ s - k
      payoff s Put = max 0.0 $ k - s
  putStrLn "Pricing ... "
  let π = ana g $ Seed s 0.0 :: Tree Double -- Very weird that I have to provide sig here.
          where g :: Seed -> Base (Tree Double) Seed
                g (Seed s t) | t > t_T = error "δt must divide equally into T"
                g (Seed s t) | t < t_T = 
                  BranchF 
                    (payoff s claim)
                    Seed {s = u * s, t = t + δt} 
                    Seed {s = v * s, t = t + δt}
                g (Seed s t) | t == t_T = LeafF s
  putStrLn . show $ π
