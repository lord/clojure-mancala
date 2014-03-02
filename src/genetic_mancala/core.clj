(ns genetic-mancala.core)

(defn get-random-move
  "Gets a random move for the current player, making sure to not choose a empty square"
  [state]
  (let [{:keys [board move]} state]
    ()))

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
     :turn (if (and (= (mod (+ house seeds offset) 6) 0) (not= (+ house seeds offset) 0))
             turn
             (mod (+ turn 1) 2))}))


(make-move {:board [0 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 0} 0)

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
  (mod (eval-bot (:board state) bot) 6))

(get-move {:board [3 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 0} [:rand])

(get-move {:board [3 3 3 12 3 3 3 3 3 3 3 3 3 3 3] :turn 0} [:sub 6 [:val 3]])

(defn play-game
  "Plays two bots against each other"
  [bot0 bot1]
  (loop [state {:board [3 3 3 3 3 3 0 3 3 3 3 3 3 0] :turn 0} iterations 0]
  ; (loop [state {:board [1 0 0 0 0 0 16 1 0 0 0 0 0 16] :turn 0} iterations 0]
    (let [final-scores (get-final-scores state)
          turn (state :turn)
          current-bot (nth [bot0 bot1] turn)]
      (if (and (= final-scores false) (< iterations 1000000))
        (recur (make-move state (get-move state current-bot)) (inc iterations))
        ; state))))
        final-scores))))

(defn calc-fitness
  "Calculate a bot's fitness"
  [bot]
  (+ (nth (play-game bot [:rand]) 0)
     (nth (play-game [:rand] bot) 1)))

(calc-fitness [0])
