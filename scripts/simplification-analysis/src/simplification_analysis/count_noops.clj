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
  (count (re-seq #"no(-)?op" str)))

(defn count-noops-for-file
  [data-file]
  (let [lines (s/split-lines (slurp data-file))
        genome-line (first (filter #(s/starts-with? % "Genome:") lines))
        program-line (first (filter #(s/starts-with? % "Program:") lines))
        total-train-error-line (first (filter #(s/starts-with? % "Total Train Error:") lines))
        total-test-error-line (first (filter #(s/starts-with? % "Total Test Error:") lines))
        simplified-genome-lines (filter #(s/starts-with? % "Simplified Genome:") lines)
        simplified-program-lines (filter #(s/starts-with? % "Simplified Program:") lines)]
      {:initial-genome-noop-count (count-noops genome-line)
       :initial-program-noop-count (count-noops program-line)
       :total-train-error (read-string (nth (s/split total-train-error-line #" ") 3))
       :total-test-error (read-string (nth (s/split total-test-error-line #" ") 3))
       :simplified-genome-noop-counts (frequencies (map count-noops simplified-genome-lines))
       :simplified-program-noop-counts (frequencies (map count-noops simplified-program-lines))}))

(defn count-noops-for-files
  [data-files]
  (map count-noops-for-file data-files))
