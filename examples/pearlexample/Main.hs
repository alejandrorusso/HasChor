module Main (main) where

import System.IO
import Data.Word

foreign import ccall "allocate_socket_" allocateSocket :: IO Int
foreign import ccall "connect_" connect :: Int -> IO ()
foreign import ccall "authenticate_" authenticate :: Int -> Word32 -> IO Int

-- getToken :: KnownSymbol client => Proxy client -> Choreo m (Bool @ client)

main :: IO ()
main = do
    socket <- allocateSocket
    connect socket
    putStr "enter the secret>"
    hFlush stdout
    x <- getLine
    authenticate socket (read x :: Word32)
    putStrLn "done"