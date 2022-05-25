
## ekg-prometheus-adapter

Simple library which maps an EKG's `Store` into a [prometheus](http://hackage.haskell.org/package/prometheus) `Registry`,

`forkBoth` makes both EKG & Prometheus available at the given port.  You can find the EKG status page at path `/` and Prometheus metrics at `/metrics`.  Metrics registered with EKG will also be exposed as Prometheus metrics.  No mapping is performed the other way, so this is only useful for migrating from EKG to Prometheus.

### Example

The following example demonstrates how you can use this library to expose to Prometheus various
GHC RTS's metrics whilst still defining your application-specific metrics.

``` haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.Metrics.Prometheus.Concurrent.Registry
  ( registerCounter, registerGauge, registerHistogram
  )
import System.Metrics.Prometheus.MetricId
import System.Random (randomIO, randomRIO)
import System.Remote.Monitoring.Prometheus (Server(..), defaultOptions, forkBoth)
import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as C
import qualified System.Metrics.Prometheus.Metric.Gauge as G
import qualified System.Metrics.Prometheus.Metric.Histogram as H

main :: IO ()
main = do
  metricsServer <- forkBoth (defaultOptions mempty) 8080
  metrics <- applicationMetrics metricsServer
  -- pass metrics around to code that will update the custom metrics
  forever $ do
    randomValues metrics
    threadDelay 1_000_000 -- 1 second

applicationMetrics :: Server -> IO AppMetrics
applicationMetrics Server{ekgStore, prometheusRegistry} = do
    -- Labels can be defined as lists or added to an empty label set
    connectSuccessGauge <- registerGauge "example_connections" (fromList [("login", "success")]) prometheusRegistry
    connectFailureGauge <- registerGauge "example_connections" (addLabel "login" "failure" mempty) prometheusRegistry
    connectCounter <- registerCounter "example_connection_total" mempty prometheusRegistry
    latencyHistogram <- registerHistogram "example_round_trip_latency_ms" mempty [10, 20..100] prometheusRegistry
    -- older EKG metrics
    errorCounter <- EKG.createCounter "errors" ekgStore -- no labels in EKG
    return AppMetrics {..}

-- | Metrics specific to this application
data AppMetrics = AppMetrics
  { connectSuccessGauge :: G.Gauge
  , connectFailureGauge :: G.Gauge
  , connectCounter :: C.Counter
  , latencyHistogram :: H.Histogram
  , errorCounter :: EKG.Counter
  }

randomValues :: AppMetrics -> IO ()
randomValues AppMetrics {..} = do
  C.inc connectCounter
  success <- randomIO
  if success
    then G.inc connectSuccessGauge
    else do
      G.inc connectFailureGauge
      err <- randomIO
      when err (EKG.inc errorCounter)
  time <- randomRIO (0, 100)
  H.observe time latencyHistogram
```
