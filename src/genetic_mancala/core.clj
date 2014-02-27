(ns genetic-mancala.core)

(defn make-move
  "Returns a new board state based on a house being moved"
  [state move]
  (let [board (:board state)
        turn  (:turn state)
        house (+ (mod move 6) (if (= turn 1) 7 0))
        seeds (nth board house)
        skip (if (= turn 1) 6 13)
        offset (if (<= house skip (+ house seeds)) 1 0) ]
    {:board (mapv (fn
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
                  (range 14)
                  board)
     :turn (if (= (mod (+ house seeds offset) 6) 0)
             turn
             (mod (+ turn 1) 2))}))


(make-move {:board [3 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 1} 3)

(defn squares-empty?
  "Checks if all the elements in squares are equal to 0"
  [squares]
  (if (every? (partial = 0) squares)
    true
    false))

(squares-empty? [0 0 0 0 1 0])

(defn get-final-scores
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


(defn eval-bot
  "Evaluate a statement from a bot"
  [board bot]
  (if (number? bot)
    bot
    (condp = (first bot)
      :add (+ (eval-bot board (nth bot 1)) (eval-bot board (nth bot 2)))
      :sub (- (eval-bot board (nth bot 1)) (eval-bot board (nth bot 2)))
      :val (nth board (eval-bot board (nth bot 1)))
      :rand (rand-int 6)
      (first bot)
      )))

(defn get-move
  "Gets a move from a bot based on the current state of the board"
  [state bot]
  (eval-bot (:board state) bot))

(get-move {:board [3 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 0} [:rand])

(get-move {:board [3 3 3 12 3 3 3 3 3 3 3 3 3 3 3] :turn 0} [:sub 3 [:val 3]])

(defn play-game
  "Plays two bots against each other"
  [bot0 bot1]
  (loop [state {:board [3 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 0} turn-count 0]
    (let [final-scores (get-final-scores state)
          turn (state :turn)
          current-bot (nth [bot0 bot1] turn)]
      (if (and (= final-scores false) (> 100000 turn-count))
        (recur (make-move state (get-move state current-bot)) (inc turn-count))
        ; (println (make-move state (get-move state current-bot)))
        final-scores))))
        ; final-scores))))


(play-game [:rand] [:rand])
