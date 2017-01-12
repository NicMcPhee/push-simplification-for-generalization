(ns simplification-analysis.count-noops
  (require [clojure.string :as s]))

;;; General format (that we care about here)
;;; "Genome:" is start of line with the initial genome
;;; "Program:" is start of line with the initial program
;;; "Genome Size (length):" is size of genome
;;; "Program size (points):" is size of program
;;; "Total Train Error:" is initial training error
;;; "Total Test Error:" is initial test (generalization) error

;;; "Simplified Genome:"
;;; "Simplified Genome Size (length):"
;;; "Simplified Program:"
;;; "Simplified Program Size (points):"
;;; "Simplified Total Train Error:"
;;; "Simplified Total Test Error:"

(defn count-noops
  [str]
  (count (re-seq #"noop" str)))

(defn count-noops-for-file
  [data-file]
  (let [lines (s/split-lines (slurp data-file))
        genome-lines (filter #(s/starts-with? % "Genome:") lines)
        simplified-lines (filter #(s/starts-with? % "Simplified Genome:") lines)]
      {:initial-noop-count (count-noops (first genome-lines))
       :simplified-noop-counts (frequencies (map count-noops simplified-lines))}))

(defn count-noops-for-files
  [data-files]
  (map count-noops-for-file data-files))
