{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import           System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
  transmitter <- socket Push
  connect transmitter "tcp://127.0.0.1:5553"

  receiver <- socket Pull
  bind receiver "tcp://*:5554"

  liftIO $ putStrLn "Sending zero..."
  send transmitter [] "0"
  _ <- receive receiver
  liftIO $ putStrLn "Answer received! Ready to go..."

  forever $ do
    answer <- receive receiver
    send transmitter [] answer