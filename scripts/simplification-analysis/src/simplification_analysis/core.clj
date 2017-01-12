(ns simplification-analysis.core
  (require [simplification-analysis.count-noops :as cn])
  (:gen-class))

(defn -main
  "Count the number of introduced NOOPs in the given output files"
  [& output-files]
  (doseq [r (cn/count-noops-for-files output-files)]
    (println r)))
