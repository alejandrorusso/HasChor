{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -fplugin=Choreography.Plugin #-}
module Main (main) where

import Choreography
import Data.Proxy

import GHC.TypeLits

$(compileFor 2 [ ("person1", ("localhost", 4242))
               , ("person2", ("localhost", 4343))
               , ("person3", ("localhost", 4344))
               , ("server",  ("localhost", 4345))
               ])

averageSalary :: Choreo IO (() @ "server")
averageSalary = do
  v1 <- locally person1 $ \_ -> return "50"
  r1 <- (person1, v1) ~> server

  v2 <- locally person2 $ \_ -> return "90"
  r2 <- (person2, v2) ~> server

  v3 <- locally person3 $ \_ -> return "75"
  r3 <- (person3, v3) ~> server

  avg <- locally server $ \un -> return $ div ((read $ take 2 $ un r1) + (read $ un r2) + (read $ un r3)) 3

  c1 <- (server, avg) ~> person1
  c2 <- (server, avg) ~> person2
  c3 <- (server, avg) ~> person3

  locally person1 $ \un -> putStrLn $ show $ un c1
  locally person2 $ \un -> putStrLn $ show $ un c2
  locally person3 $ \un -> putStrLn $ show $ un c3

  locally server $ \_ -> return ()

main :: IO ()
main = run' averageSalary