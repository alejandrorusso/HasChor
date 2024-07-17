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

$(compileFor 0 [ ("a", ("localhost", 4224))
               , ("b", ("localhost", 4225))])

data Edge = forall l l' . (KnownSymbol l, KnownSymbol l') => Edge (Proxy l) (Proxy l')

e :: Edge
e = Edge a b

{-# SPECIALISE f e #-}
f :: Edge -> Choreo IO ()
f (Edge x z) = do
  x `locally` \_ -> putStrLn "at a"
  z `locally` \_ -> putStrLn "at b"
  return ()

main :: IO ()
main = run' $ f e


-- $(compileFor 0 [ ("node1", ("localhost", 4242))
-- --               , ("node2", ("localhost", 4422))
--                , ("server", ("localhost", 4343))
--                ])

-- {-# NOINLINE addNumber #-}
-- -- {-# SPECIALISE forall v r . addNumber node1 v r #-}
-- -- {-# SPECIALISE forall v r . addNumber node2 v r #-}
-- addNumber :: KnownSymbol l => Proxy l -> Int @ l -> IORef Int @ "server" -> Choreo IO (() @ "server")
-- addNumber l vAtl ref = do
--   vAtServer <- (l, vAtl) ~> server
--   server `locally` \uw ->
--     modifyIORef (uw ref) (\a -> a + uw vAtServer)

-- choreography :: Choreo IO (() @ "server")
-- choreography = do
--   n1v <- node1 `locally` \_ -> return 50
--   --n2v <- node2 `locally` \_ -> return 75
--   sref <- server `locally` \_ -> newIORef 0

--   addNumber node1 n1v sref
--   --addNumber node2 n2v sref

--   server `locally` \uw -> do
--     v <- readIORef (uw sref)
--     putStrLn $ show v

-- main = run' choreography
