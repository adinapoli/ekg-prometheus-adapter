{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
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

import Control.Concurrent.MVar
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable
import qualified Data.HashMap.Strict as HMap
import Data.Int
import qualified Data.Map.Strict as Map
import Lens.Micro.TH
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Distribution as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus
import System.Metrics.Prometheus.Registry (RegistrySample)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT(..))

--------------------------------------------------------------------------------
data AdapterOptions = AdapterOptions {
    _labels :: Prometheus.Labels
  , _namespace :: Maybe T.Text
  }

makeLenses ''AdapterOptions

--------------------------------------------------------------------------------
data Distribution = Distribution
  { meanG :: Gauge.Gauge
  , varianceG :: Gauge.Gauge
  , countC :: Counter.Counter
  , sumG :: Gauge.Gauge
  , minG :: Gauge.Gauge
  , maxG :: Gauge.Gauge
  }

data Metric =
    C Counter.Counter
  | G Gauge.Gauge
  | D Distribution

deriving newtype instance Hashable Prometheus.Name

type MetricsMap = HMap.HashMap Prometheus.Name Metric

--------------------------------------------------------------------------------
defaultOptions :: Prometheus.Labels -> AdapterOptions
defaultOptions l = AdapterOptions l Nothing

--------------------------------------------------------------------------------
registerEKGStore :: MonadIO m =>
  EKG.Store -> AdapterOptions -> m (Prometheus.Registry, IO RegistrySample)
registerEKGStore store opts = do
  (registry, mmap) <- liftIO $ toPrometheusRegistry' store opts
  mv <- liftIO $ newMVar mmap
  return (registry, updateMetrics store opts registry mv >> Prometheus.sample registry)

--------------------------------------------------------------------------------
toPrometheusRegistry' :: EKG.Store -> AdapterOptions -> IO (Prometheus.Registry, MetricsMap)
toPrometheusRegistry' store opts = do
  registry <- Prometheus.new
  samples <- EKG.sampleAll store
  mmap <- HMap.fromList . catMaybes <$> traverse (mkMetric opts registry) (HMap.toList samples)
  return (registry, mmap)

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> AdapterOptions -> IO Prometheus.Registry
toPrometheusRegistry store opts = fst <$> toPrometheusRegistry' store opts

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions -> Prometheus.Registry -> (T.Text, EKG.Value) -> IO (Maybe (Prometheus.Name, Metric))
mkMetric AdapterOptions{..} registry (key, value) = do
  let k = mkKey _namespace key
  case value of
   EKG.Counter c -> do
     counter <- Prometheus.registerCounter k _labels registry
     Counter.add (fromIntegral c) counter
     return (Just (k, C counter))
   EKG.Gauge g   -> do
     gauge <- Prometheus.registerGauge k _labels registry
     Gauge.set (fromIntegral g) gauge
     return (Just (k, G gauge))
   EKG.Label _   -> return Nothing
   EKG.Distribution stats -> do
     let statGauge name = do
           gauge <- Prometheus.registerGauge k ( Prometheus.addLabel "stat" name _labels) registry
           return gauge
     meanG <- statGauge "mean"
     varianceG <- statGauge"variance"
     countC <- Prometheus.registerCounter k (Prometheus.addLabel "stat" "count" _labels) registry
     sumG <- statGauge"sum"
     minG <- statGauge"min"
     maxG <- statGauge"max"
     let distribution = Distribution {..}
     updateDistribution distribution stats
     return (Just (k, D Distribution {..}))

updateDistribution :: Distribution -> EKG.Stats -> IO ()
updateDistribution Distribution{..} stats = do
  Gauge.set (EKG.mean stats) meanG
  Gauge.set (EKG.variance stats) varianceG
  Gauge.set (EKG.sum stats) sumG
  Gauge.set (EKG.min stats) minG
  Gauge.set (EKG.max stats) maxG
  updateCounter countC (EKG.count stats)

updateCounter :: Counter.Counter -> Int64 -> IO ()
updateCounter counter c = do
  (Counter.CounterSample oldCounterValue) <- Counter.sample counter
  let slack = c - fromIntegral oldCounterValue
  when (slack >= 0) $ Counter.add (fromIntegral slack) counter
--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> Prometheus.Registry -> MVar MetricsMap -> IO ()
updateMetrics store opts registry mmap = do
  samples <- EKG.sampleAll store
  traverse_ (updateMetric opts registry mmap) (HMap.toList samples)

--------------------------------------------------------------------------------
mkKey :: Maybe T.Text -> T.Text -> Prometheus.Name
mkKey mbNs k =
  -- TODO forbid digit as first character
  Prometheus.Name $ ns <> T.map replace_ k
  where
    ns = maybe mempty (<> "_") mbNs
    allowed c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
    replace_ c = if allowed c then c else '_'

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> Prometheus.Registry -> MVar MetricsMap -> (T.Text, EKG.Value) -> IO ()
updateMetric opts@AdapterOptions{..} registry mv kv@(key, value) = do
  let k = mkKey _namespace key
  mmap <- readMVar mv
  case HMap.lookup k mmap of
    Just metric -> updateMetric' metric value
    Nothing -> modifyMVar_ mv $ \mmap -> do
      -- take the MMap MVar before the Registry, so we never register two metrics for the same EKG
      m_metric <- mkMetric opts registry kv
      case m_metric of
        Nothing -> return mmap
        Just (_, metric) -> do
          updateMetric' metric value
          return $ HMap.insert k metric mmap
  where
    updateMetric' :: Metric -> EKG.Value -> IO ()
    updateMetric' metric value = case (metric, value) of
      (C counter, EKG.Counter c)  -> updateCounter counter c
      (G gauge,   EKG.Gauge g) -> Gauge.set (fromIntegral g) gauge
      (D distribution, EKG.Distribution stats) -> updateDistribution distribution stats
      _ -> return ()
