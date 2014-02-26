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


(make-move {:board [3 3 3 12 3 3 3 3 3 3 3 3 3 3 3] :turn 0} 3 0)

(defn squares-empty?
  "Checks if all the elements in squares are equal to 0"
  [squares]
  (if (< 0 (reduce #(if (> %1 %2) %1 %2) squares))
    false
    true))

(squares-empty? [0 0 0 0 1 0])

(defn final-scores
  "Checks if a game is over, returns final scores if it is, otherwise false"
  [state]
  (let [board (:board state)
        p0-houses (take 6 board)
        p1-houses (subvec board 7 13)]
    (if (or (squares-empty? p0-houses)
            (squares-empty? p1-houses))
      [(+ (nth board 06) (reduce + p0-houses))
       (+ (nth board 13) (reduce + p1-houses))]
      false)))
