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
  , defaultOptions
  , updateMetrics
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import           Data.Monoid
import Lens.Micro.TH
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus
import System.Metrics.Prometheus.Registry (RegistrySample)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT(..), runRegistryT)

--------------------------------------------------------------------------------
data AdapterOptions = AdapterOptions {
    _labels :: Prometheus.Labels
  , _namespace :: Maybe T.Text
  }

makeLenses ''AdapterOptions

--------------------------------------------------------------------------------
data Metric =
    C Counter.Counter
  | G Gauge.Gauge

type MetricsMap = Map.Map Prometheus.Name Metric

--------------------------------------------------------------------------------
defaultOptions :: Prometheus.Labels -> AdapterOptions
defaultOptions l = AdapterOptions l Nothing

--------------------------------------------------------------------------------
registerEKGStore :: MonadIO m => EKG.Store -> AdapterOptions -> RegistryT m () -> m (IO RegistrySample)
registerEKGStore store opts registryAction = do
  (registry, mmap) <- liftIO $ toPrometheusRegistry' store opts
  runReaderT (unRegistryT registryAction) registry
  return (updateMetrics store opts mmap >> Prometheus.sample registry)

--------------------------------------------------------------------------------
toPrometheusRegistry' :: EKG.Store -> AdapterOptions -> IO (Prometheus.Registry, MetricsMap)
toPrometheusRegistry' store opts = do
  registry <- Prometheus.new
  samples <- EKG.sampleAll store
  mmap <- foldM (mkMetric opts registry) Map.empty (HMap.toList samples)
  return (registry, mmap)

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> AdapterOptions -> IO Prometheus.Registry
toPrometheusRegistry store opts = fst <$> toPrometheusRegistry' store opts

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions -> Prometheus.Registry -> MetricsMap -> (T.Text, EKG.Value) -> IO MetricsMap
mkMetric AdapterOptions{..} registry mmap (key, value) = do
  let k = mkKey _namespace key
  case value of
   EKG.Counter c -> do
     counter <- Prometheus.registerCounter k _labels registry
     Counter.add (fromIntegral c) counter
     return $! Map.insert k (C counter) $! mmap
   EKG.Gauge g   -> do
     gauge <- Prometheus.registerGauge k _labels registry
     Gauge.set (fromIntegral g) gauge
     return $! Map.insert k (G gauge) $! mmap
   EKG.Label _   -> return $! mmap
   EKG.Distribution _ -> return $! mmap

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> MetricsMap -> IO ()
updateMetrics store opts mmap = do
  samples <- EKG.sampleAll store
  traverse_ (updateMetric opts mmap) (HMap.toList samples)

--------------------------------------------------------------------------------
mkKey :: Maybe T.Text -> T.Text -> Prometheus.Name
mkKey mbNs k =
  Prometheus.Name $ (maybe mempty (\x -> x <> "_") mbNs) <> T.replace "." "_" k

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO ()
updateMetric AdapterOptions{..} mmap (key, value) = do
  let k = mkKey _namespace key
  case (Map.lookup k mmap, value) of
    -- TODO if we don't have a metric registered, register one
    (Just (C counter), EKG.Counter c)  -> do
      (Counter.CounterSample oldCounterValue) <- Counter.sample counter
      let slack = c - fromIntegral oldCounterValue
      when (slack >= 0) $ Counter.add (fromIntegral slack) counter
    (Just (G gauge),   EKG.Gauge g) -> do
      Gauge.set (fromIntegral g) gauge
    _ -> return ()
