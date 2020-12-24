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
import Control.Comonad.Cofree (Cofree)
import Control.Comonad (Comonad, Comonad(..))

data Tree a 
  = Leaf a 
  | Branch {
      value :: a,
      up :: Tree a,
      down :: Tree a
    } deriving (Show, Functor)

instance Comonad Tree where
  extract (Leaf a) = a
  extract (Branch a _ _) = a
  duplicate x = Leaf x

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

price :: Params -> Double
price θ@Params{..} = histo calcPrice . (ana calcPayoff :: Seed -> Tree Double) $ Seed s_0 0.0
  where calcPayoff :: Seed -> Base (Tree Double) Seed
        calcPayoff Seed{t}        | t > t_T = error "δt must divide equally into T"
        calcPayoff θ_t@Seed{s, t} | t < t_T = 
          BranchF 
            (payoff θ θ_t) --(π (u * s) (v * s))
            Seed {s = u * s, t = t + δt}
            Seed {s = v * s, t = t + δt}
        calcPayoff θ_t@Seed{s, t} | t == t_T = LeafF (payoff θ θ_t)

        calcPrice :: Base (Tree Double) (Cofree (Base (Tree Double)) Double) ->  Double
        calcPrice (LeafF payoff) = payoff
        calcPrice (BranchF _ up down) = π (extract up) (extract down) -- this is surprising: price doesn't depend on payoff at t < T.
        π π_up π_down = disc * (p' * π_up + (1.0 - p') * π_down)
        p' = 0.5 + r * sqrt(δt) / 2.0 / σ
        disc = 1.0 / (1.0 + r * δt)
        δt = t_T / int2Double n
