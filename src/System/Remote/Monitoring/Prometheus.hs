{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module System.Remote.Monitoring.Prometheus
  ( toPrometheusRegistry
  , registerEKGStore
  , registerEKGStore'
  , AdapterOptions(..)
  , labels
  , namespace
  , samplingFrequency
  , defaultOptions
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Lens.Micro.TH
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT(..))

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
registerEKGStore store opts = RegistryT $ ReaderT $ \registry ->
  liftIO $ registerEKGStore' store opts registry

--------------------------------------------------------------------------------
-- | Like 'registerEKGStore', but takes the concurrent 'Prometheus.Registry'
-- explicitly. Useful when the registry handle is managed outside of
-- 'RegistryT' (e.g. shared with other components of the application).
registerEKGStore' :: EKG.Store -> AdapterOptions -> Prometheus.Registry -> IO ()
registerEKGStore' store opts registry = do
  mmap <- toPrometheusRegistry' registry store opts
  void $ forkIO $ forever $ do
    threadDelay (_samplingFrequency opts * (1_000_000 :: Int))
    updateMetrics store opts mmap

--------------------------------------------------------------------------------
toPrometheusRegistry' :: Prometheus.Registry
                      -> EKG.Store
                      -> AdapterOptions
                      -> IO MetricsMap
toPrometheusRegistry' registry store opts = do
  samples <- EKG.sampleAll store
  foldM (mkMetric opts registry) Map.empty (HMap.toList samples)

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> AdapterOptions -> IO Prometheus.Registry
toPrometheusRegistry store opts = do
  registry <- Prometheus.new
  toPrometheusRegistry' registry store opts
  pure registry

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions
         -> Prometheus.Registry
         -> MetricsMap
         -> (T.Text, EKG.Value)
         -> IO MetricsMap
mkMetric AdapterOptions{..} registry mmap (!key, !value) = do
  let k = mkKey _namespace key
  case value of
   EKG.Counter c -> do
     !counter <- Prometheus.registerCounter k _labels registry
     Counter.add (fromIntegral c) counter
     pure $! Map.insert k (C counter) $! mmap
   EKG.Gauge g   -> do
     !gauge <- Prometheus.registerGauge k _labels registry
     Gauge.set (fromIntegral g) gauge
     pure $! Map.insert k (G gauge) $! mmap
   EKG.Label _   -> pure $! mmap
   EKG.Distribution _ -> pure $! mmap

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> MetricsMap -> IO ()
updateMetrics store opts mmap = do
  samples <- EKG.sampleAll store
  forM_ (HMap.toList samples) (updateMetric opts mmap)


--------------------------------------------------------------------------------
mkKey :: Maybe T.Text -> T.Text -> Prometheus.Name
mkKey mbNs k =
  Prometheus.Name $ (maybe mempty (\x -> x <> "_") mbNs) <> T.replace "." "_" k

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO ()
updateMetric AdapterOptions{..} mmap (!key, !value) = do
  let k = mkKey _namespace key
  case Map.lookup k mmap of
    Just (C counter)
     | EKG.Counter c <- value
     -> do (Counter.CounterSample oldCounterValue) <- Counter.sample counter
           let slack = c - fromIntegral oldCounterValue
           when (slack >= 0) $ Counter.add (fromIntegral slack) counter
    Just (G gauge)
      | EKG.Gauge g <- value
      -> Gauge.set (fromIntegral g) gauge
    _ -> pure ()
