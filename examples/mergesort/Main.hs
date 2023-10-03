{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.Proxy
import Data.Time
import GHC.TypeLits (KnownSymbol)
import System.Environment

-- * ROBERT: Does not work, as sort and merge are both location polymorphic and recursive

divide :: [a] -> ([a], [a])
divide xs = splitAt lhx xs
  where
    lhx = length xs `div` 2

$(compileFor 0         [ ("primary", ("localhost", 3000))
                       , ("worker1", ("localhost", 4000))
                       , ("worker2", ("localhost", 5000))
                       ])

{-# INLINE sort #-}
sort ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  KnownSymbol c =>
  Proxy c ->
  ([Int] @ a) ->
  Choreo IO ([Int] @ a)
sort a b c lst = do
  condition <- a `locally` \unwrap -> do return $ length (unwrap lst) > 1
  cond (a, condition) \case
    True -> do
      pivot <- a `locally` \unwrap -> do return $ length (unwrap lst) `div` 2
      divided <- a `locally` \unwrap -> do return $ divide (unwrap lst)
      l <- a `locally` \unwrap -> do return $ fst (unwrap divided)
      r <- a `locally` \unwrap -> do return $ snd (unwrap divided)
      l' <- (a, l) ~> b
      r' <- (a, r) ~> c
      ls' <- sort b c a l'
      rs' <- sort c a b r'
      merge a b c ls' rs'
    False -> do
      return lst

{-# INLINE merge #-}
merge ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  KnownSymbol c =>
  Proxy c ->
  [Int] @ b ->
  [Int] @ c ->
  Choreo IO ([Int] @ a)
merge a b c lhs rhs = do
  lhsHasElements <- b `locally` \unwrap -> do return $ not (null (unwrap lhs))
  cond (b, lhsHasElements) \case
    True -> do
      rhsHasElements <- c `locally` \unwrap -> do return $ not (null (unwrap rhs))
      cond (c, rhsHasElements) \case
        True -> do
          rhsHeadAtC <- c `locally` \unwrap -> do return $ head (unwrap rhs)
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b
          takeLhs <- b `locally` \unwrap -> do return $ head (unwrap lhs) <= unwrap rhsHeadAtB
          cond (b, takeLhs) \case
            True -> do
              -- take (head lhs) and merge the rest
              lhs' <- b `locally` \unwrap -> do return $ tail (unwrap lhs)
              merged <- merge a b c lhs' rhs
              lhsHeadAtB <- b `locally` \unwrap -> do return $ head (unwrap lhs)
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a
              a `locally` \unwrap -> do return $ unwrap lhsHeadAtA : unwrap merged
            False -> do
              -- take (head rhs) and merge the rest
              rhs' <- c `locally` \unwrap -> do return $ tail (unwrap rhs)
              merged <- merge a b c lhs rhs'
              rhsHeadAtC <- c `locally` \unwrap -> do return $ head (unwrap rhs)
              rhsHeadAtA <- (c, rhsHeadAtC) ~> a
              a `locally` \unwrap -> do return $ unwrap rhsHeadAtA : unwrap merged
        False -> do
          (b, lhs) ~> a
    False -> do
      (c, rhs) ~> a

mainChoreo :: Choreo IO ()
mainChoreo = do
  lst <- primary `locally` \unwrap -> do return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  primary `locally` \unwrap -> do
    print (unwrap sorted)
    return ()
  return ()

main :: IO ()
main = run' mainChoreo

-- main :: IO ()
-- main = do
--   [loc] <- getArgs
--   case loc of
--     "primary" -> runChoreography config mainChoreo "primary"
--     "worker1" -> runChoreography config mainChoreo "worker1"
--     "worker2" -> runChoreography config mainChoreo "worker2"
--   return ()
--   where
--     config =
--       mkHttpConfig
--         [ ("primary", ("localhost", 3000)),
--           ("worker1", ("localhost", 4000)),
--           ("worker2", ("localhost", 5000))
--         ]
