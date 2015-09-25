(ns genetic-tsp-clj.core
  (:require [clojure.math.numeric-tower :as math]
            (incanter [core :as ic]
                      [charts :as icc])))

(defn square [a] (math/expt a 2))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

;; a route is defined as a vector of places
;; mutation on the route must be done carefully, we need to go to every city, so
;; we can't take anything out, only shuffle.
(defn mutate [individual] (shuffle individual))

;; crossover needs to enforce the same constraint, so use this funky mechanism
;; take 0->n from parentA (hope the good subset is in here)
;; iterate over all x in Parent. if x not in Offspring, append x to the earliest
;; unfilled position in the offspring, if x in Offspring, skip it
(defn crossover
  [parentA parentB]
  {:pre [(= (count parentA) (count parentB))]
   :post [(and
            (= (count %) (count parentA))
            (nil? (some nil? %)))]} ;; some weird bug causes this to happen sometimes

  (let
    [split-point (+ 1 (rand-int (- (count parentA) 1))) ;; random between 0-A
     subset      (subvec parentA 0 split-point)
     bwithoutsubset (filter #(not (in? subset %)) parentB)]
    (reduce conj subset bwithoutsubset)))

(defn distance
  [a b]
  (math/sqrt (+
              (square (- (first a)  (first b)))
              (square (- (second a) (second b))))))

(defn fitness
  [individual]
  ; (println "individual:" individual)
  (loop [prev   (first individual)
         remain (rest individual)
         fitness 0.0]
    (if (empty? remain)
      fitness
      (recur
        (first remain)
        (rest remain)
        (+ fitness (distance (first remain) prev))))))

(defn fitness-cmp
  [a b]
  (< (fitness a) (fitness b)))

(defn generate-problem
  "creates a problem with n elements"
  [n m]
  (take n (map vector
               (repeatedly #(rand-int m))
               (repeatedly #(rand-int m)))))

(defn make-population
  [problem size]
  (map mutate (repeat size problem)))

(defn sort-population [population] (sort fitness-cmp population))

(defn select-by-tournament
  "takes t-size random elements from population and keeps fittest"
  [population t-size]
  (first (sort-population (take t-size (shuffle population)))))

(defn build-new-population
  [population t-size mutation-rate]
  (map #(if (< (rand) mutation-rate)
          (mutate %)
          %)
       (take (count population) (repeatedly #(crossover
                                               (select-by-tournament population t-size)
                                               (select-by-tournament population t-size))))))

(defn solve-problem-serial
  "solves a problem. stops when we hit iteration limit or fitness changes less than tolerance"
  ([problem itereration-limit pop-size t-size mut-rate]
   (loop [i            0
          population   (make-population problem pop-size)]
     (let
       [best (first (sort-population population))
        bf   (fitness best)]

       (println "iteration:" i)
       (println "best fitness:" bf)

       (if (= i (- itereration-limit 1))
         best
         (recur
           (+ 1 i)
           (build-new-population population t-size mut-rate)))))))

(defn run
  [{:keys [num-cities max-coord iter-limit pop-size t-size mut-rate]}]
  (let
    [
     problem  (generate-problem num-cities max-coord)
     solution (solve-problem-serial problem iter-limit pop-size t-size mut-rate)
     xs       (map first solution)
     ys       (map second solution)
     ]
    (do
      (println problem)
      (def plot (icc/scatter-plot))
      (icc/add-points plot xs ys)
      (icc/add-lines plot xs ys)
      (ic/view plot)
      )))
