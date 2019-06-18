 (ns Treasure.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-map []
  (with-open [rdr (clojure.java.io/reader "map.txt")]
    (def matrix (reduce conj [] (line-seq rdr))))
  (vec (map (fn [arg] (clojure.string/split arg #"")) matrix)))

(def rows (count (read-map)))
(def columns (count (first (read-map))))

(defn checkvalidity [vectormap length]
  (if (= (count vectormap) 0)
    true
    (and (= (count (first vectormap)) length) (checkvalidity (rest vectormap) length))))


(defn mapwithflag [vectormap]
  (conj [vectormap] [false]))

(defn print-challenge []
  (def whole_map (slurp "map.txt"))
  (println "This is my Challenge :\n")
  (println whole_map)
  whole_map)

(defn starting_point []
  (def startingpoint (vector 0 0))
  startingpoint)

(defn destination_point [vectormap]
  (def positionoftreasure (filter #(some #{"@"} %) vectormap))
  (def indexofx (.indexOf vectormap (nth positionoftreasure 0)))
  (def indexofy (.indexOf (nth positionoftreasure 0) "@"))
  (def finalpoint (vector indexofx indexofy))
  finalpoint)

(defn atend? [currentx currenty]
  (= [currentx currenty] [(nth (destination_point (read-map)) 0)
                          (nth (destination_point (read-map)) 1)]))
(defn markvisited1 [position vectormap]
  (def row (nth vectormap (nth position 0)))
  (conj [(assoc vectormap (nth position 0) (assoc row (nth position 1) "+"))] [true]))

(defn markvisited [position vectormap]
  (def row (nth vectormap (nth position 0)))
  (def mylist (conj [] position))
  (if (= position (destination_point (read-map)))
    (do (conj [(assoc vectormap (nth position 0) (assoc row (nth position 1) "@"))] [true]))
    (do (if (= (nth vectormap 1) [true])
          vectormap
          (conj [(assoc vectormap (nth position 0) (assoc row (nth position 1) "!"))] [false])))))

(defn notinmap [x y]
  (or (< x 0) (< y 0)
      (>= x rows) (>= y columns)))

(defn validmove [position vectormap]
  (def result (notinmap (nth position 0) (nth position 1)))
  (if (false? result)
    (do
      (def row (nth vectormap (nth position 0)))
      (def value (nth row (nth position 1)))
      (and
       (not (= value "#"))
       (not (= value "!"))))
    false))

(defmulti move
  (fn [row col direction]
    direction))

(defmethod move :upwards [row col direction]
  [(- row 1) col])

(defmethod move :rightwards [row col direction]
  [row (+ col 1)])

(defmethod move :downwards [row col direction]
  [(+ row 1) col])

(defmethod move :leftwards [row col direction]
  [row (- col 1)])

(defn pretty-print [vectormap]
  (loop [x 0]
    (when (< x rows)
      (println (str/join (nth vectormap x)))
      (recur (+ x 1)))))

(defn solve [position vectormap]
  (def row (nth (nth vectormap 0) (nth position 0)))
  (def value (nth row (nth position 1)))

  (if (= value "@")
    (assoc vectormap 1 [true])
    (let [mynewmap (if (true? (validmove [(nth (move (nth position 0) (nth position 1) :upwards) 0)
                                          (nth (move (nth position 0) (nth position 1) :upwards) 1)] (nth vectormap 0)))
                     (solve (move (nth position 0) (nth position 1) :upwards) (markvisited position (nth vectormap 0)))
                     vectormap)]

      (if (= (nth mynewmap 1) [true])
        (markvisited1 position (nth mynewmap 0))
        (let [mynewmap (if (true? (validmove [(nth (move (nth position 0) (nth position 1) :rightwards) 0)
                                              (nth (move (nth position 0) (nth position 1) :rightwards) 1)] (nth mynewmap 0)))
                         (solve (move (nth position 0) (nth position 1) :rightwards) (markvisited position (nth mynewmap 0)))
                         mynewmap)]

          (if (= (nth mynewmap 1) [true])
            (markvisited1 position (nth mynewmap 0))
            (let [mynewmap (if (true? (validmove [(nth (move (nth position 0) (nth position 1) :leftwards) 0)
                                                  (nth (move (nth position 0) (nth position 1) :leftwards) 1)] (nth mynewmap 0)))
                             (solve (move (nth position 0) (nth position 1) :leftwards) (markvisited position (nth mynewmap 0)))
                             mynewmap)]

              (if (= (nth mynewmap 1) [true])
                (markvisited1 position (nth mynewmap 0))
                (let [mynewmap (if (true? (validmove [(nth (move (nth position 0) (nth position 1) :downwards) 0)
                                                      (nth (move (nth position 0) (nth position 1) :downwards) 1)] (nth mynewmap 0)))
                                 (solve (move (nth position 0) (nth position 1) :downwards) (markvisited position (nth mynewmap 0)))
                                 mynewmap)] (if (= (nth mynewmap 1) [true])
                                              (markvisited1 position (nth mynewmap 0))
                                              (markvisited position (nth mynewmap 0))))))))))))

(defn start [vectormap]
  (if (checkvalidity vectormap (count (first vectormap)))
    (do
      (print-challenge)
      (println "\n")
      (def mymap (nth (solve [0 0] (mapwithflag (read-map))) 0))
      (def issolved (nth (solve [0 0] (mapwithflag (read-map))) 1))

      (if (true? (nth issolved 0))
        (do (println "WoW, See I found the treasure")
            (println "\n")
            (pretty-print mymap))
        (do (println "Uh oh, Sorry I couldnt find the treasure")
            (println "\n")
            (pretty-print mymap))))
    (println "+++++++++You map file is not valid, please check rows and columns++++++++")))

(start (read-map))