{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Scarch
  ( Scarch
  , runScarchIO
  , printInfo
  , printError
  , downloadFile
  , writeFileLBS
  , getYtdlJson
  , concurrently
  ) where

import Conduit (runResourceT, sinkFile)
import Control.Concurrent.Async (mapConcurrently, race_)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
       (newTQueue, readTQueue, writeTQueue)
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.Free.Church (F, iterM, liftF)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Network.HTTP.Simple (httpSink, parseRequest)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process
       (CreateProcess(std_out), StdStream(CreatePipe), createProcess,
        proc, waitForProcess)

data ScarchF :: * -> * where
  PrintInfo :: String -> a -> ScarchF a
  PrintError :: String -> a -> ScarchF a
  DownloadFile :: FilePath -> String -> a -> ScarchF a
  WriteFile :: FilePath -> Lazy.ByteString -> a -> ScarchF a
  GetMetadata :: String -> (Strict.ByteString -> a) -> ScarchF a
  Concurrently
    :: forall t a b. Traversable t
    => t (Scarch a)
    -> (t a -> b)
    -> ScarchF b

deriving instance Functor ScarchF

type Scarch = F ScarchF

runDownloadFile :: FilePath -> String -> IO ()
runDownloadFile path url = do
  createDirectoryIfMissing True (takeDirectory path)
  req <- parseRequest url
  runResourceT (httpSink req (const (sinkFile path)))

runWriteFile :: FilePath -> Lazy.ByteString -> IO ()
runWriteFile path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  Lazy.writeFile path contents

runGetMetadata :: String -> IO Strict.ByteString
runGetMetadata url = do
  (_, Just pout, _, phandle) <-
    createProcess
      (proc "youtube-dl" ["--flat-playlist", "-J", "-f", "best", url])
      {std_out = CreatePipe}
  json <- Strict.hGetContents pout
  void (waitForProcess phandle)
  return json

runScarchIO :: Scarch () -> Int -> IO ()
runScarchIO scarch numConnections = do
  o <- atomically newTQueue
  e <- atomically newTQueue
  reqSem <- newQSem numConnections
  let withReqSem :: IO a -> IO a
      withReqSem = bracket_ (waitQSem reqSem) (signalQSem reqSem)
      phi :: ScarchF (IO a) -> IO a
      phi (PrintInfo info m) = atomically (writeTQueue o info) *> m
      phi (PrintError err m) = atomically (writeTQueue e err) *> m
      phi (DownloadFile path url m) = withReqSem (runDownloadFile path url) *> m
      phi (WriteFile path contents m) = runWriteFile path contents *> m
      phi (GetMetadata url m) = withReqSem (runGetMetadata url) >>= m
      phi (Concurrently actions m) = mapConcurrently run actions >>= m
      run :: forall a. Scarch a -> IO a
      run = iterM phi
  race_
    (race_
       (forever (atomically (readTQueue o) >>= putStrLn))
       (forever (atomically (readTQueue e) >>= hPutStrLn stderr)))
    (run scarch)

printInfo :: String -> Scarch ()
printInfo info = liftF (PrintInfo info ())

printError :: String -> Scarch ()
printError err = liftF (PrintError err ())

downloadFile :: FilePath -> String -> Scarch ()
downloadFile path url = liftF (DownloadFile path url ())

writeFileLBS :: FilePath -> Lazy.ByteString -> Scarch ()
writeFileLBS path contents = liftF (WriteFile path contents ())

getYtdlJson :: String -> Scarch Strict.ByteString
getYtdlJson url = liftF (GetMetadata url id)

concurrently :: Traversable t => t (Scarch a) -> Scarch (t a)
concurrently actions = liftF (Concurrently actions id)
