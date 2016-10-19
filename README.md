
## ekg-prometheus-adapter

Simple library which maps an EKG's `Store` into a [prometheus](http://hackage.haskell.org/package/prometheus)'s `Registry`,
also exposing a function to "inject" the newly created `Registry` into your `RegistryT` computation.

### Example

The following example demostrates how you can use this library to expose to Prometheus various
GHC RTS's metrics whilst still defining your application-specific metrics.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           GHC.Conc (numSparks, getNumCapabilities, getNumProcessors)
import qualified System.Metrics as EKG
import           System.Metrics.Prometheus.Concurrent.Http (serveHttpTextMetrics)
import           System.Metrics.Prometheus.Metric.Counter (add, inc)
import           System.Metrics.Prometheus.MetricId
import           System.Metrics.Prometheus.Registry (Registry, RegistrySample)
import           System.Metrics.Prometheus.RegistryT
import           System.Random
import           System.Remote.Monitoring.Prometheus (registerEKGStore, defaultOptions)


mkRegistry store port = do
  runRegistryT $ do
    registerEKGStore store (defaultOptions $ fromList [("ghc", "rts")])

    -- Labels can be defined as lists or added to an empty label set
    connectSuccessGauge <- registerGauge "example_connections" (fromList [("login", "success")])
    connectFailureGauge <- registerGauge "example_connections" (addLabel "login" "failure" mempty)
    connectCounter <- registerCounter "example_connection_total" mempty
    latencyHistogram <- registerHistogram "example_round_trip_latency_ms" mempty [10, 20..100]

    liftIO $ forkIO $ do
      let loop = forever $ do
            threadDelay (5 * 10^6)
            v <- randomRIO (1,5)
            add v connectCounter
            loop
      loop

    sample >>= serveHttpTextMetrics port ["metrics"]

main :: IO ()
main = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  -- Add GHC.Conc metrics
  EKG.registerGauge "ghc.conc.num_sparks" (fromIntegral <$> numSparks) store
  EKG.registerCounter "ghc.conc.num_capabilities" (fromIntegral <$> getNumCapabilities) store
  EKG.registerCounter "ghc.conc.num_processors" (fromIntegral <$> getNumProcessors) store
  _ <- mkRegistry store 8080
  return ()
```
