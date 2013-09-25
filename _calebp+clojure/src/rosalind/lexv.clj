(ns rosalind.lexv
  (:require [clojure.math.combinatorics :as cmb]))

(defn make-comparator [alphabet]
  (let [order-values (zipmap alphabet (range))
        compare-chars #(.compareTo (order-values %1)
                                   (order-values %2))]
    (fn [a b]
      (or (->> (map compare-chars a b)
               (remove zero?)
               first)
          0))))

(defn create-strings [alphabet n]
  (->> (range 1 (inc n))
       (mapcat (partial cmb/selections alphabet))
       (sort (make-comparator alphabet))
       (map #(apply str %))))

(assert
 (= ["D" "DD" "DDD" "DDN" "DDA" "DN" "DND" "DNN" "DNA" "DA" "DAD" "DAN" "DAA" "N" "ND" "NDD" "NDN" "NDA" "NN" "NND" "NNN" "NNA" "NA" "NAD" "NAN" "NAA" "A" "AD" "ADD" "ADN" "ADA" "AN" "AND" "ANN" "ANA" "AA" "AAD" "AAN" "AAA"]
    (create-strings "DNA" 3)))

(defn run [alphabet n]
  (doseq [s (create-strings alphabet n)]
    (println s)))
