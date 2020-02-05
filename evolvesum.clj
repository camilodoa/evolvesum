(ns evolvesum.core)

;; evolve a vector of 100 0's and 1's
;; an individual is a vector of 100 random bits

(defn new-individual []
  "Creates a vector of 100 integers that are either 0 or 1"
  (repeatedly 100 #(rand-int 2)))

;; an individual is mutated by possibly flipping each bit.

(defn mutate [individual]
  "Randomly mutates individual by flipping their bit"
  (for [b individual]
    (if (> (rand) 0.01) ;;rand = btwn 0 and 1 (most of time > 0.01)
      b           ;; if statement above is true return b
      (- 1 b))))  ;; flip bit

;; the error of an individual is the difference between the sum of the bits
;; and the goal, which we hardcode here.

(defn error
  "Error of an individual; goal is 73 1s"
  [individual]
  (Math/abs (- (reduce + individual) 73)))

;; an individual is better than another if it has lower error.
;; two arguments are two individuals
;; true if first argument is better than the second

(defn better
  "Compares two individuals"
  [i1 i2]
  (< (error i1) (error i2)))

;; we evolve a solution by starting with a random population and repeatedly
;; sorting, checking for a solution, and producing a new population.
;; we produce the new population by selecting and mutating the better half
;; of the current population.

(defn evolve
  "Evolutionary Algorithm"
  [popsize]
  (loop [generation 0
         population (sort better (repeatedly popsize new-individual))]
    ;; sort takes a function that takes 2 args and return true if first arg comes before second
    ;; best one is at beginning and worst is at end
    (let [best (first population)]
      (println "Generation:" generation ", Best error:" (error best))
      (if (zero? (error best))
        (println "Success:" best)
        (let [better-half (take (int (/ popsize 2)) population)] ;;else
          ;; better-half is a new vector of the first half of pop
          (recur
            (inc generation)
            (sort better (map mutate
                              (concat better-half better-half)))))))))

;; run with a population of 100

(evolve 100)
