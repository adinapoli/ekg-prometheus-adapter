
module System.Remote.Monitoring.Prometheus
  ( toPrometheusRegistry
  ) where

import           Control.Monad
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import qualified System.Metrics.Prometheus.Registry as Prometheus


toPrometheusRegistry :: EKG.Store -> Prometheus.Labels -> IO Prometheus.Registry
toPrometheusRegistry store labels = do
  let registry = Prometheus.new
  samples <- EKG.sampleAll store
  foldM (mkMetric labels) registry (HMap.toList samples)

mkMetric :: Prometheus.Labels -> Prometheus.Registry -> (T.Text, EKG.Value) -> IO Prometheus.Registry
mkMetric labels oldRegistry (key, value) = do
  case value of
   EKG.Counter c -> do
     (counter, newRegistry) <- Prometheus.registerCounter (Prometheus.Name key) labels oldRegistry
     Counter.add (fromIntegral c) counter
     return newRegistry
   EKG.Gauge g   -> do
     (gauge, newRegistry) <- Prometheus.registerGauge (Prometheus.Name key) labels oldRegistry
     Gauge.set (fromIntegral g) gauge
     return newRegistry
   EKG.Label _   -> return oldRegistry
   EKG.Distribution _ -> return oldRegistry
