(ns genetic-mancala.core)

(defn skip [player] (if (= player 1) 13 6))

(defn make-move
  "Returns a new board state based on a house being moved"
  [state house player]
  (let [board (:board state)
        seeds (nth board house)
        skip (if (= player 1) 6 13)
        turn (:turn state)
        offset (if (<= house skip (+ house seeds)) 1 0) ]
    {:board (map-indexed (fn
                    [i house-seeds]
                    (cond
                      ; remove seeds from house where you picked up
                      (= house i)
                      0
                      (= skip i)
                      house-seeds
                      ; drop seeds down around the board
                      (or (<= house i (+ house seeds offset)) (<= house (+ i 14) (+ house seeds offset)))
                      (+ 1 house-seeds)
                      :else
                      house-seeds))
                  board)
     :turn (if (= (mod (+ house seeds offset) 6) 0)
             turn
             (mod (+ turn 1) 2))}))


(make-move {:board [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] :turn 0} 3 0)
