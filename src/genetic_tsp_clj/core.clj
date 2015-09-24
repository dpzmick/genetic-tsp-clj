(ns genetic-tsp-clj.core
  (:gen-class))

;; a route is defined as a vector of places
;; mutation on the route must be done carefully, we need to go to every city, so
;; we can't take anything out, only shuffle.
(defn mutate [individual] (shuffle individual))

;; crossover needs to enforce the same constraint, so use this funky mechanism
;; select some random subset S_(i, j) \in A
;; set Offspring_(i,j) = S_(i,j)
;; iterate over all x in Parent. if x not in Offspring, append x to the earliest
;; unfilled position in the offspring, if x in Offspring, skip it
(defn crossover
  [parentA parentB]
  (if (= (count parentA) (count parentB))
    (let
      [lower-bound (rand-int (count parentA))
       upper-bound (+ lower-bound (rand-int (- (count parentA) lower-bound)))
       subset      (subvec parentA lower-bound upper-bound)
       offspring   (flatten
                     [
                      (take lower-bound (repeat nil))
                      subset
                      (take (- (count parentA) upper-bound) (repeat nil))
                      ])]
      offspring)
    (Exception. "parents must be the same size")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
