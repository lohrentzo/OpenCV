{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Monad
import qualified OpenCV as CV
import           System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as B
import           Control.Concurrent.Async hiding (async)

cvDisplay :: ZMQ z ()
cvDisplay = do
  transmitter <- socket Push
  connect transmitter "tcp://127.0.0.1:5552"

  receiver <- socket Pull
  bind receiver "tcp://*:5554"

  liftIO $ putStrLn "cvDisplay: sending zero..."
  send transmitter [] "0"
  _ <- receive receiver
  liftIO $ putStrLn "cvDisplay: answer received! Ready to go..."

  let loop = do
        buffer <- receive receiver
        let img = CV.imdecode CV.ImreadUnchanged buffer
        key <- liftIO . CV.withWindow "Video" $ \window -> do
          CV.imshow window img
          CV.waitKey 10
        when (key /= 27) loop
  loop

cvProc :: ZMQ z ()
cvProc = do
  trToCap <- socket Push
  connect trToCap "tcp://127.0.0.1:5553"

  recFromCap <- socket Pull
  bind recFromCap "tcp://*:5555"  

  trToDis <- socket Push
  connect trToDis "tcp://127.0.0.1:5554"

  recFromDis <- socket Pull
  bind recFromDis "tcp://*:5552"

  liftIO $ putStrLn "cvProc: waiting for zero from cvCap..."
  _ <- receive recFromCap
  liftIO $ putStrLn "cvProc: zero received!"
  send trToCap [] "0"
  liftIO $ putStrLn "cvProc: waiting for zero from cvDisplay..."
  _ <- receive recFromDis
  send trToDis [] "0"
  liftIO $ putStrLn "cvProc: zero received! Ready to go..."

  forever $ do
    buffer <- receive recFromCap
    if buffer == "0"
    then send trToCap [] "0"
    else do
      let frame = CV.imdecode CV.ImreadUnchanged buffer
      let newFrame = frame -- The famous "do nothing" process
      let newBuffer = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) newFrame
      send trToDis [] newBuffer

cvCap :: ZMQ z ()
cvCap = do
    transmitter <- socket Push
    connect transmitter "tcp://127.0.0.1:5555"

    receiver <- socket Pull
    bind receiver "tcp://*:5553"

    liftIO $ putStrLn "cvCap: sending zero..."
    send transmitter [] "0"
    _ <- receive receiver
    liftIO $ putStrLn "cvCap: answer received! Ready to go..."
    
    cap <- liftIO CV.newVideoCapture
    liftIO $ CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    isOpened <- liftIO $ CV.videoCaptureIsOpened cap

    if isOpened then loop transmitter cap else liftIO $ putStrLn "cvCap: couldn't open video capture device"
  where
    loop transmitter cap = do
      _ok <- liftIO $ CV.videoCaptureGrab cap
      mbImg <- liftIO $ CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
            let buffer = CV.exceptError $ CV.imencode png img
            send transmitter [] buffer
            loop transmitter cap 
          where
            png = CV.OutputPng CV.defaultPngParams
        Nothing -> pure ()

main :: IO ()
main = runZMQ $ do
  async cvProc
  async cvCap
  s <- async cvDisplay
  liftIO $ wait s
  liftIO $ putStrLn "Done."