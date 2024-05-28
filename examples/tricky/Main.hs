{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Choreography
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.Proxy
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Environment
import Data.IORef

-- $(compileFor 1 [ ("node1", ("localhost", 4242))
--                , ("node2", ("localhost", 4422))
--                , ("server", ("localhost", 4343))
--                ])

handle :: Proxy "node2" -> Proxy "server" -> Proxy "node1"
handle = error "handle should not be evaluated"

node1 :: Proxy "node1"
node1 = Proxy
node2 :: Proxy "node2"
node2 = Proxy
server :: Proxy "server"
server = Proxy

{-# RULES "CHOREORULES locallynode2"
              forall (f::(forall a. At a "node2" -> a) -> m a)
                     (node2::Proxy "node2"). node2 `locally` f
                = wrap <$> run (f unwrap) #-}
{-# RULES "CHOREORULES locallynode1"
              forall (f::(forall a. At a "node1" -> a) -> m a)
                     (node1::Proxy "node1"). node1 `locally` f
                = return mkEmpty #-}
{-# RULES "CHOREORULES locallyserver"
              forall (f::(forall a. At a "server" -> a) -> m a)
                     (server::Proxy "server"). server `locally` f
                = return mkEmpty #-}
{-# RULES "CHOREORULES sendfromnode2"
              forall v recipient (node2::Proxy "node2"). (node2, v) ~> recipient
                = (send (unwrap v) (toLocTm recipient)) >> return mkEmpty #-}
{-# RULES "CHOREORULES sendtonode2"
              forall from v (node2::Proxy "node2"). (from, v) ~> node2
                = wrap <$> recv (toLocTm from) #-}
{-# RULES "CHOREORULES sendnode1server"
              forall v (node1::Proxy "node1") (server::Proxy "server"). (node1, 
                                                                         v)
                                                                          ~> server
                = return mkEmpty #-}
{-# RULES "CHOREORULES sendservernode1"
              forall v (server::Proxy "server") (node1::Proxy "node1"). (server, 
                                                                         v)
                                                                          ~> node1
                = return mkEmpty #-}
{-# RULES "CHOREORULES sendIdnode2"
              forall v (node2::Proxy "node2"). (node2, v) ~> node2
                = return (wrap (unwrap v)) #-}
{-# RULES "CHOREORULES condnode2"
              forall v c (node2::Proxy "node2"). (node2, v) `cond` c
                = (broadcast (unwrap v)) >> c (unwrap v) #-}
{-# RULES "CHOREORULES condnode1"
              forall v c (node1::Proxy "node1"). (node1, v) `cond` c
                = (recv (toLocTm node1)) >>= \ x -> c x #-}
{-# RULES "CHOREORULES condserver"
              forall v c (server::Proxy "server"). (server, v) `cond` c
                = (recv (toLocTm server)) >>= \ x -> c x #-}
run'_a3Er choreo_a3Es
  = (runChoreography
       (mkHttpConfig
          [("node1", ("localhost", 4242)), ("node2", ("localhost", 4422)),
           ("server", ("localhost", 4343))])
       choreo_a3Es "node2"
       >> return ())


{-# NOINLINE addNumber #-}
{-# SPECIALISE forall v r . addNumber node1 v r #-}
{-# SPECIALISE forall v r . addNumber node2 v r #-}
addNumber :: KnownSymbol l => Proxy l -> Int @ l -> IORef Int @ "server" -> Choreo IO (() @ "server")
addNumber l vAtl ref = do
  vAtServer <- (l, vAtl) ~> server
  server `locally` \uw ->
    modifyIORef (uw ref) (\a -> a + uw vAtServer)

choreography :: Choreo IO (() @ "server")
choreography = do
  n1v <- node1 `locally` \_ -> return 50
  n2v <- node2 `locally` \_ -> return 75
  sref <- server `locally` \_ -> newIORef 0

  addNumber node1 n1v sref
  addNumber node2 n2v sref

  server `locally` \uw -> do
    v <- readIORef (uw sref)
    putStrLn $ show v

main = run'_a3Er choreography