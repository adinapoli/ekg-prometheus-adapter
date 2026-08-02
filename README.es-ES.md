

## ekg-prometheus-adapter

Biblioteca simple que mapea un `Store` de EKG a un `Registry` de [prometheus](http://hackage.haskell.org/package/prometheus), exponiendo también una función para "inyectar" el `Registry` recién creado en tu computación `RegistryT`.

### Ejemplo

El siguiente ejemplo demuestra cómo puedes usar esta biblioteca para exponer a Prometheus diversas métricas del RTS de GHC, mientras aún defines tus propias métricas específicas de la aplicación.

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

    -- Las etiquetas pueden definirse como listas o agregarse a un conjunto de etiquetas vacío
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
  -- Agregar métricas de GHC.Conc
  EKG.registerGauge "ghc.conc.num_sparks" (fromIntegral <$> numSparks) store
  EKG.registerCounter "ghc.conc.num_capabilities" (fromIntegral <$> getNumCapabilities) store
  EKG.registerCounter "ghc.conc.num_processors" (fromIntegral <$> getNumProcessors) store
  _ <- mkRegistry store 8080
  return ()
```
