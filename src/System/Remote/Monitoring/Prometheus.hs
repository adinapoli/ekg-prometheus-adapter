{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module System.Remote.Monitoring.Prometheus
  ( toPrometheusRegistry
  , registerEKGStore
  , AdapterOptions(..)
  , labels
  , namespace
  , samplingFrequency
  , defaultOptions
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import           Data.Monoid
import Lens.Micro.TH
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Registry as Prometheus
import           System.Metrics.Prometheus.RegistryT (RegistryT(..))

--------------------------------------------------------------------------------
data AdapterOptions = AdapterOptions {
    _labels :: Prometheus.Labels
  , _namespace :: Maybe T.Text
  , _samplingFrequency :: !Int
    -- ^ How often update the registry (in seconds).
  }

makeLenses ''AdapterOptions

--------------------------------------------------------------------------------
data Metric =
    C Counter.Counter
  | G Gauge.Gauge

type MetricsMap = Map.Map Prometheus.Name Metric

--------------------------------------------------------------------------------
defaultOptions :: Prometheus.Labels -> AdapterOptions
defaultOptions l = AdapterOptions l Nothing 15

--------------------------------------------------------------------------------
registerEKGStore :: MonadIO m => EKG.Store -> AdapterOptions -> RegistryT m ()
registerEKGStore store opts = RegistryT $ StateT $ \_ -> do
  (r, mmap) <- liftIO $ toPrometheusRegistry' store opts
  liftIO $ forkIO $ do
    let loop = forever $ do
                 threadDelay (_samplingFrequency opts * 10^6)
                 updateMetrics store opts mmap
                 loop
    loop
  return ((), r)

--------------------------------------------------------------------------------
toPrometheusRegistry' :: EKG.Store -> AdapterOptions -> IO (Prometheus.Registry, MetricsMap)
toPrometheusRegistry' store opts = do
  let registry = Prometheus.new
  samples <- EKG.sampleAll store
  foldM (mkMetric opts) (registry, Map.empty) (HMap.toList samples)

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> AdapterOptions -> IO Prometheus.Registry
toPrometheusRegistry store opts = fst <$> toPrometheusRegistry' store opts

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions -> (Prometheus.Registry, MetricsMap) -> (T.Text, EKG.Value) -> IO (Prometheus.Registry, MetricsMap)
mkMetric AdapterOptions{..} (oldRegistry, mmap) (key, value) = do
  let k = mkKey _namespace key
  case value of
   EKG.Counter c -> do
     (counter, newRegistry) <- Prometheus.registerCounter k _labels oldRegistry
     Counter.add (fromIntegral c) counter
     return $! (newRegistry, Map.insert k (C counter) $! mmap)
   EKG.Gauge g   -> do
     (gauge, newRegistry) <- Prometheus.registerGauge k _labels oldRegistry
     Gauge.set (fromIntegral g) gauge
     return $! (newRegistry, Map.insert k (G gauge) $! mmap)
   EKG.Label _   -> return $! (oldRegistry, mmap)
   EKG.Distribution _ -> return $! (oldRegistry, mmap)

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> MetricsMap -> IO ()
updateMetrics store opts mmap = do
  samples <- EKG.sampleAll store
  const () <$> foldM (updateMetric opts) mmap (HMap.toList samples)


--------------------------------------------------------------------------------
mkKey :: Maybe T.Text -> T.Text -> Prometheus.Name
mkKey mbNs k =
  Prometheus.Name $ (maybe mempty (\x -> x <> "_") mbNs) <> T.replace "." "_" k

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO MetricsMap
updateMetric AdapterOptions{..} mmap (key, value) = do
  let k = mkKey _namespace key
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
