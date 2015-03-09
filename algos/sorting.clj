(ns algos.sorting)


(defn insert [xs x]
  (cond
    (empty? xs) (list x)
    (< x (first xs)) (conj xs x)
    :else (conj (insert (rest xs) x) (first xs))))

(defn insertion-sort [sorted xs]
  (if
    (empty? xs) sorted
    (insertion-sort (insert sorted (first xs)) (rest xs))))

(println (insertion-sort (list) (list 7 6 1 2 3 5))) 
