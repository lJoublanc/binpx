{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- Example of pricing using the Binomial theorem
module Main where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Float (int2Double)

data Tree a 
  = Leaf a 
  | Branch {
      value :: a,
      up :: Tree a,
      down :: Tree a
    } deriving Show

makeBaseFunctor ''Tree

data Claim = Call | Put

data Params 
  = Params {
    s_0 :: Double,
    σ :: Double,
    r :: Double,
    k :: Double,
    n :: Int, -- used to calc δt
    t_T :: Double,
    u :: Double,
    v :: Double,
    payoff :: Params -> Seed -> Double
  }

data Seed = Seed {
  s :: Double,
  t :: Double
}
  
main = do
  putStrLn "Pricing ... "
  let π = price Params {
            s_0 = 100.0,
            σ = 0.2,
            r = 0.05,
            k = 100.0,
            n = 4,
            t_T = 1.0,
            u = 1.1,
            v = 0.9,
            payoff = \Params{k} Seed{s} -> max 0.0 (s - k)
          }
  putStrLn . show $ π

price :: Params -> Tree Double
price θ@Params{..} = ana g $ Seed s_0 0.0
  where g :: Seed -> Base (Tree Double) Seed
        g Seed{t}        | t > t_T = error "δt must divide equally into T"
        g θ_t@Seed{s, t} | t < t_T = 
          BranchF 
            (payoff θ θ_t)
            Seed {s = u * s, t = t + δt}
            Seed {s = v * s, t = t + δt}
        g θ_t@Seed{s, t} | t == t_T = LeafF (payoff θ θ_t)

        δt = t_T / int2Double n
        p' = 0.5 + r * sqrt(δt) / 2.0 / σ
        disc = 1.0 / (1.0 + r * δt)

