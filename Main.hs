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
    } deriving (Show, Functor)

makeBaseFunctor ''Tree

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
  putStrLn . show . price $ Params {
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

-- Calculte price using binary tree. TODO: This is ineffective as n² calculations are done.
price :: Params -> Double
price θ@Params{..} = hylo calcPrice calcPayoff $ Seed s_0 0.0
  where calcPayoff :: Seed -> Base (Tree Double) Seed
        calcPayoff Seed{t}        | t > t_T = error "δt must divide equally into T"
        calcPayoff θ_t@Seed{s, t} | t < t_T = 
          BranchF 
            (payoff θ θ_t)
            Seed {s = u * s, t = t + δt}
            Seed {s = v * s, t = t + δt}
        calcPayoff θ_t@Seed{s, t} | t == t_T = LeafF (payoff θ θ_t)

        calcPrice :: Base (Tree Double) Double ->  Double
        calcPrice (LeafF payoff) = payoff
        calcPrice (BranchF _ up down) = π up down -- this is surprising: price doesn't depend on payoff at t < T.
        π up down = disc * (p' * up + (1.0 - p') * down)
        p' = 0.5 + r * sqrt(δt) / 2.0 / σ
        disc = 1.0 / (1.0 + r * δt)
        δt = t_T / int2Double n
