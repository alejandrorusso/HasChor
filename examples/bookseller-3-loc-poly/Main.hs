{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Choreography
import Data.Proxy
import Data.Time
import GHC.TypeLits
import System.Environment

$(compileFor 0 [ ("buyer", ("localhost", 4242))
               , ("seller", ("localhost", 4343))
               ])

-- {-# SPECIALISE bookseller :: Proxy "buyer" -> Choreo IO (Maybe Day @ "buyer") #-}
-- {-# SPECIALISE forall . bookseller buyer #-}
-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes the name of the buyer as a parameter (`someBuyer`).
bookseller :: KnownSymbol a => Proxy a -> Choreo IO (Maybe Day @ a)
bookseller someBuyer = do
  -- the buyer reads the title of the book and sends it to the seller
  title <- (someBuyer, \_ -> do
               putStrLn "Enter the title of the book to buy"
               getLine
           )
           ~~> seller

  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un title)) ~~> someBuyer

  cond' (someBuyer, \un -> return $ (un price) < budget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un title)) ~~> someBuyer

      someBuyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un deliveryDate)
        return $ Just (un deliveryDate)

    False -> do
      someBuyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01

main :: IO ()
main = run' $ bookseller buyer