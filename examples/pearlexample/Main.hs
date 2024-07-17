{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import System.IO
import Data.Word

import GHC.TypeLits
import Choreography
import Data.Proxy

foreign import ccall "allocate_socket_" allocateSocket :: IO Int
foreign import ccall "connect_"         connect        :: Int -> IO ()
foreign import ccall "authenticate_"    authenticate   :: Int -> Word32 -> IO Word64
foreign import ccall "close_"           close          :: Int -> IO ()

$(compileFor 1 [ ("client1",        ("localhost", 4242))
               , ("authenticator", ("localhost", 4343))
               ])

{-# SPECIALISE forall . getAccessToken client1 #-}
getAccessToken :: KnownSymbol l => Proxy l -> Choreo IO (Maybe Word64 @ l)
getAccessToken p = do
    authSocket <- authenticator `locally` \_ -> do
        socket <- allocateSocket
        {- upon connection, the remote server is going to present the
        user with a secret. -}
        connect socket
        return socket

    secret <- p `locally` \_ -> do
        putStr "enter the secret>"
        hFlush stdout
        getLine
    
    authSecret <- (p, secret) ~> authenticator

    authRes <- authenticator `locally` \uw -> do
        r <- authenticate (uw authSocket) (read (uw authSecret) :: Word32)
        close (uw authSocket)
        if r == 0 then return Nothing else return (Just r)
    
    pToken <- (authenticator, authRes) ~> p

    p `locally` \uw -> putStrLn $ show $ uw pToken

    return pToken

main :: IO ()
main = run' $ getAccessToken client1