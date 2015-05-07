(ns lion.core)

(def candidates [:bilbo 
                 :sauruman 
                 :zobboo 
                 :frodo 
                 :rosana
                 :strider
                 :taco])
(def candidate-count (count candidates))

(defn generate-ballots
  [id]
  (let [take-num (inc (rand-int candidate-count))
        the-candidates (take take-num (shuffle candidates))]
    (loop [current-ballot []
           current-candidate (first the-candidates)
           rest-candidates (next the-candidates)
           current-number 1]
      (if (empty? rest-candidates)
        current-ballot
        (recur (conj current-ballot {:voter id 
                                     :pref current-number 
                                     :name current-candidate})
               (first rest-candidates)
               (next rest-candidates)
               (inc current-number))))))

(defn generate-random-votes
  [how-many]
  (loop [how-many how-many
         votes []]
    (if (= how-many 0)
      votes
      (let [ballots (generate-ballots how-many)]
        (recur (dec how-many) (into votes ballots))))))

(defn get-current-votes
  [ballots]
  (map first ballots))

(defn group-ballots
  [ballots]
  (vals (group-by :voter (sort-by :pref < ballots))))

(defn sort-votes
  [votes]
  (let [vote-vector (vec (group-by :name votes))
        names (map first vote-vector) 
        counts (map (comp count last) vote-vector)
        sorted-votes (sort-by last > (map vector names counts))]
    sorted-votes))

(defn count-votes
  [sorted-votes]
  (let [current-winner-votes (last (first sorted-votes))
        total-votes (reduce + (map last sorted-votes))]
    (if (> (/ current-winner-votes total-votes) 0.5)
      [true (ffirst sorted-votes)]
      [false (first (last sorted-votes))])))

(defn get-sorted-votes
  [ballots]
  (->> (group-ballots ballots)
       get-current-votes
       sort-votes))

(defn remove-ballots
  [ballots candidate-name]
  (filter #(not= (:name %) candidate-name) ballots))

(defn print-winner
  [winner] 
  (println (str "The winner is: " winner))
  winner)

(defn print-elimination
  [eliminated]
  (println (str eliminated " is eliminated")))

(defn simulate-election
  [ballots]
  (loop [ballots ballots]
    (let [sorted (get-sorted-votes ballots)
          counted (count-votes sorted)]
      (println (str "Current votes: " sorted))
      (if (first counted)
        (print-winner (last counted))
        (do (print-elimination (last counted)) 
            (recur (remove-ballots ballots (last counted))))))))

(simulate-election (generate-random-votes 100))
