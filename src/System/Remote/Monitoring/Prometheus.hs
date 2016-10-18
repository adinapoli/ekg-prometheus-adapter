{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module System.Remote.Monitoring.Prometheus
  ( toPrometheusRegistry
  , registerEKGStore
  , AdapterOptions(..)
  , defaultOptions
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Registry as Prometheus
import           System.Metrics.Prometheus.RegistryT (RegistryT(..))

--------------------------------------------------------------------------------
data AdapterOptions = AdapterOptions {
    labels :: Prometheus.Labels
  , samplingFrequency :: !Int
    -- ^ How often update the registry (in seconds).
  }

--------------------------------------------------------------------------------
data Metric =
    C Counter.Counter
  | G Gauge.Gauge

type MetricsMap = Map.Map Prometheus.Name Metric

--------------------------------------------------------------------------------
defaultOptions :: Prometheus.Labels -> AdapterOptions
defaultOptions l = AdapterOptions l 15

--------------------------------------------------------------------------------
registerEKGStore :: MonadIO m => EKG.Store -> AdapterOptions -> RegistryT m ()
registerEKGStore store AdapterOptions{..} = RegistryT $ StateT $ \_ -> do
  (r, mmap) <- liftIO $ toPrometheusRegistry' store labels
  liftIO $ forkIO $ do
    let loop = forever $ do
                 threadDelay (samplingFrequency * 10^6)
                 updateMetrics store mmap
                 loop
    loop
  return ((), r)

--------------------------------------------------------------------------------
toPrometheusRegistry' :: EKG.Store -> Prometheus.Labels -> IO (Prometheus.Registry, MetricsMap)
toPrometheusRegistry' store labels = do
  let registry = Prometheus.new
  samples <- EKG.sampleAll store
  foldM (mkMetric labels) (registry, Map.empty) (HMap.toList samples)

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> Prometheus.Labels -> IO Prometheus.Registry
toPrometheusRegistry store labels = fst <$> toPrometheusRegistry' store labels

--------------------------------------------------------------------------------
mkMetric :: Prometheus.Labels -> (Prometheus.Registry, MetricsMap) -> (T.Text, EKG.Value) -> IO (Prometheus.Registry, MetricsMap)
mkMetric labels (oldRegistry, mmap) (key, value) = do
  let k = Prometheus.Name key
  case value of
   EKG.Counter c -> do
     (counter, newRegistry) <- Prometheus.registerCounter k labels oldRegistry
     Counter.add (fromIntegral c) counter
     return $! (newRegistry, Map.insert k (C counter) $! mmap)
   EKG.Gauge g   -> do
     (gauge, newRegistry) <- Prometheus.registerGauge (Prometheus.Name key) labels oldRegistry
     Gauge.set (fromIntegral g) gauge
     return $! (newRegistry, Map.insert k (G gauge) $! mmap)
   EKG.Label _   -> return $! (oldRegistry, mmap)
   EKG.Distribution _ -> return $! (oldRegistry, mmap)

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> MetricsMap -> IO ()
updateMetrics store mmap = do
  samples <- EKG.sampleAll store
  const () <$> foldM updateMetric mmap (HMap.toList samples)

--------------------------------------------------------------------------------
updateMetric :: MetricsMap -> (T.Text, EKG.Value) -> IO MetricsMap
updateMetric mmap (key, value) = do
  let k = Prometheus.Name key
  case liftM2 (,) (Map.lookup k mmap) (Just value) of
    Just (C counter, EKG.Counter c)  -> do
      (Counter.CounterSample oldCounterValue) <- Counter.sample counter
      let slack = c - fromIntegral oldCounterValue
      when (slack >= 0) $ Counter.add (fromIntegral slack) counter
      return $! mmap
    Just (G gauge,   EKG.Gauge g) -> do
      Gauge.set (fromIntegral g) gauge
      return $! mmap
    _ -> return mmap
