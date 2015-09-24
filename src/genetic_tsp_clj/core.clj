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
       offspring   (flatten
                     [(take lower-bound (repeat nil))
                      subset
                      (take (- (count parentA) upper-bound) (repeat nil))])]
      (populate-offspring offspring parentB))
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
