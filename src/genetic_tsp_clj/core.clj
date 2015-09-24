(ns genetic-tsp-clj.core
  (:require [clojure.math.numeric-tower :as math]))

(defn sq [a] (math/expt a 2))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

;; a route is defined as a vector of places
;; mutation on the route must be done carefully, we need to go to every city, so
;; we can't take anything out, only shuffle.
(defn mutate [individual] (shuffle individual))

;; crossover needs to enforce the same constraint, so use this funky mechanism
;; select some random subset S_(i, j) \in A
;; set Offspring_(i,j) = S_(i,j)
;; iterate over all x in Parent. if x not in Offspring, append x to the earliest
;; unfilled position in the offspring, if x in Offspring, skip it

;; TODO this is probably horribly inefficient
(defn populate-offspring
  "fills in nils in an offspring using the given parent"
  ([offspring parent] (populate-offspring [] offspring parent))
  ([acc rem-off rem-parent]
   (if (empty? rem-off)
     acc
     (if (nil? (first rem-off))
       ;; find first parent value not in offspring
       (let
         [remaining-parent (filter #(not (in? (concat acc rem-off) %)) rem-parent)]
         (recur (conj acc (first remaining-parent)) (rest rem-off) remaining-parent))
       (recur (conj acc (first rem-off)) (rest rem-off) rem-parent)))))

(defn crossover
  [parentA parentB]
  (if (= (count parentA) (count parentB))
    (let
      [lower-bound (rand-int (count parentA))
       upper-bound (+ lower-bound (rand-int (- (count parentA) lower-bound)))
       subset      (subvec parentA lower-bound upper-bound)
       offspring   (concat
                     (take lower-bound (repeat nil))
                     subset
                     (take (- (count parentA) upper-bound) (repeat nil)))]
      (populate-offspring offspring parentB)
      )
    (Exception. "parents must be the same size")))

(defn distance
  [a b]
  (math/sqrt (+
              (sq (- (first a)  (first b)))
              (sq (- (second a) (second b))))))

(defn fitness
  [individual]
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
   (loop [i 0
          population (make-population problem pop-size)]

     (if (= i itereration-limit)
       (first (sort-population population))
       (recur (+ 1 i) (build-new-population population t-size mut-rate))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
