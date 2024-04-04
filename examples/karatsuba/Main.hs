{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
-- import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import System.Environment

import Control.Monad.Fix

-- still get some http error...

$(compileFor 0 [ ("primary", ("localhost", 4243))
               , ("worker1", ("localhost", 4342))
               , ("worker2", ("localhost", 4343))
               ])

data KaratsubaNums = KaratsubaNums
  { splitter :: Integer,
    h1 :: Integer,
    h2 :: Integer,
    l1 :: Integer,
    l2 :: Integer
  }

{-# SPECIALISE forall . karatsuba primary worker1 worker2 #-}
{-# SPECIALISE forall . karatsuba primary worker2 worker1 #-}
{-# SPECIALISE forall . karatsuba worker1 primary worker2 #-}
{-# SPECIALISE forall . karatsuba worker2 primary worker1 #-}
{-# SPECIALISE forall . karatsuba worker1 worker2 primary #-}
{-# SPECIALISE forall . karatsuba worker2 worker1 primary #-}
karatsuba ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
  Proxy a ->
  Proxy b ->
  Proxy c ->
  (Integer @ a) ->
  (Integer @ a) ->
  Choreo IO (Integer @ a)
karatsuba a b c n1 n2 = do
  done <- a `locally` \unwrap -> return $ unwrap n1 < 10 || unwrap n2 < 10
  cond
    (a, done)
    \case
      True -> do
        a `locally` \unwrap -> return $ unwrap n1 * unwrap n2
      False -> do
        x <- a `locally` \unwrap -> return $ f (unwrap n1) (unwrap n2)
        l1' <- (a, \unwrap -> return $ l1 (unwrap x)) ~~> b
        l2' <- (a, \unwrap -> return $ l2 (unwrap x)) ~~> b
        h1' <- (a, \unwrap -> return $ h1 (unwrap x)) ~~> c
        h2' <- (a, \unwrap -> return $ h2 (unwrap x)) ~~> c
        z0' <- karatsuba b c a l1' l2'
        z0 <- (b, z0') ~> a
        z2' <- karatsuba c a b h1' h2'
        z2 <- (c, z2') ~> a
        s1 <- a `locally` \unwrap -> return $ l1 (unwrap x) + h1 (unwrap x)
        s2 <- a `locally` \unwrap -> return $ l2 (unwrap x) + h2 (unwrap x)
        z1' <- karatsuba a b c s1 s2
        z1 <- a `locally` \unwrap -> return $ unwrap z1' - unwrap z2 - unwrap z0
        a `locally` \unwrap -> return let s = splitter (unwrap x) in (unwrap z2 * s * s) + (unwrap z1 * s) + unwrap z0
        where
          f n1 n2 = KaratsubaNums {splitter = splitter, h1 = h1, l1 = l1, h2 = h2, l2 = l2}
            where
              log10 :: Integer -> Double
              log10 = logBase 10 . fromIntegral
              m = max (log10 n1) (log10 n2) + 1
              m2 = floor (m / 2)
              splitter = 10 ^ m2
              h1 = n1 `div` splitter
              l1 = n1 `mod` splitter
              h2 = n2 `div` splitter
              l2 = n2 `mod` splitter

mainChoreo :: Integer -> Integer -> Choreo IO ()
mainChoreo n1 n2 = do
  n1 <- primary `locally` \_ -> return n1
  n2 <- primary `locally` \_ -> return n2
  result <- karatsuba primary worker1 worker2 n1 n2
  primary `locally` \unwrap -> do
    print (unwrap result)
    return ()
  return ()

main :: IO ()
main = do
  [n1,n2] <- map read <$> getArgs
  run' (mainChoreo n1 n2)