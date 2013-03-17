(ns clojure)
 
(def characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
 
(defn random-char []
  (char (rand-nth characters)))
 
(defn random-string [length]
  (apply str 
    (repeatedly length random-char)))
 
(def goal "METHINKS IT IS LIKE A WEASEL")
(def parent (random-string (count goal)))
(def litter-size 100)
(def rate 0.04)
 
(defn mutate-char 
  "Return the parameter or a random character,
  depending on mutation rate."
  [c]
  (if (< (rand) rate)
    (random-char)
    c))
 
(defn mutate [parent]
  (apply str
    (map mutate-char parent)))
 
(defn procreate
  "Generates a list of progeny from a given
  parent, based on the litter-size."
  [parent]
  (repeatedly litter-size
    #(mutate parent)))
 
(defn fitness 
  "Determines the fitness of a given string 
  compared to the goal string, returns an 
  integer which corresponds to the number of
  character slots identical to the goal"
  [current goal]
  (let [pairs (map vector current goal)]
    (reduce
      #(if (= (first %2) (second %2)) (inc %1) %1) 0 pairs)))
 
(defn fit-map 
  "Generates a map of strings and their fitness
  to be used by find-fittest."
  [litter goal]
  (reduce 
    #(assoc %1 %2 (fitness %2 goal)) {} litter))
 
(defn find-fittest [litter goal]
  (let [m (fit-map litter goal)]
    (key (first (reverse (sort-by val m))))))
 
(defn main [& args]
  (loop [p parent c 0]
    (println "Current string:" p)
    (let [litter (procreate p)
          fittest (find-fittest litter goal)]
      (if (= goal fittest)
        (println "It took" c "generations.")
        (recur fittest (inc c))))))
