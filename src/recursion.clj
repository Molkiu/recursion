(ns recursion)

(defn product [coll]
  (if(empty? coll)
    1
    (*(first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (== (count coll) 1))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last(rest coll))))

(defn max-element [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? tail)
        head
        (max head (max-element tail)))))

(defn seq-max [seq-1 seq-2]
  (let [a (count seq-1)
       b (count seq-2)]
    (if (> a b)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? tail)
        head
        (seq-max head (longest-sequence tail)))))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? head) (cons head (my-filter pred? tail))
      :else (my-filter pred? tail))))

(defn sequence-contains? [elem a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
  (cond
    (empty? a-seq) false
    (empty? tail) (== elem head)
    (not (== elem head)) (sequence-contains? elem tail)
    :else false)))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? head) (cons head (my-take-while pred? tail))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? head) (my-drop-while pred? tail)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [a (count a-seq)
       b (count b-seq)]
    (if (== a b)
      (every? true? (map == a-seq b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (let [seqy [seq-1 seq-2]
        head-1 (first seq-1)
        head-2 (first seq-2)
        tail-1 (rest seq-1)
        tail-2 (rest seq-2)]
    (if (some empty? seqy)
      '()
      (cons (f head-1 head-2)
            (my-map f tail-1 tail-2)))))

(defn power [n k]
  (cond
    (== k 0) 1
    (== k 1) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+(fib (- n 1))
              (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (== up-to 0) '()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) [()]
    :else (cons (seq a-seq) (tails(rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq) [()]
    :else (reverse (cons (seq a-seq) (reverse (inits (take (- (count a-seq) 1) a-seq)))))))

(defn rotations [a-seq]
  (if (seq a-seq)
    (map
     (fn [n _]
       (lazy-cat (drop n a-seq) (take n a-seq)))
     (iterate inc 0) a-seq)
    (list )))

(defn my-frequencies-helper [freqs a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (update-in freqs [head] (fnil inc 0)) tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [[x n]] (repeat n x)) a-map))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) (seq coll)
    (>= n (count coll)) (seq coll)
    :else (my-take (dec n) (butlast coll))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))
        full (count a-seq)]
    (vector (my-take ( - full middle) a-seq)
            (my-drop middle a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [af (first a-seq)
        bf (first b-seq)
        ar (rest a-seq)
        br (rest b-seq)]
    (lazy-seq
      (cond
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< af bf) (cons af (seq-merge ar b-seq))
        :else     (cons bf (seq-merge a-seq br))))))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1) a-seq
    :else (let [[p q] (halve a-seq)]
            (seq-merge (merge-sort p)
                       (merge-sort q)))))

(defn monotonics-helper [coll]
  (let [small-count (inc (count (filter true? (map <= coll (next coll)))))
        big-count (inc (count (filter false? (map <= coll (next coll)))))]
    (cond
      (empty? coll) '()
      :else (if (< small-count big-count)
              (if (first (filter true? (map <= coll (next coll))))
                (take small-count coll)
                (drop big-count coll))
              (if (first (filter false? (map <= coll (next coll))))
                (drop small-count coll)
                (drop small-count coll))))))

(defn split-into-monotonics [a-seq]
  ())

(monotonics-helper [0 1 4 5 7 3 2 1])
(monotonics-helper [0 5 4 7 1 3])
(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


(map <= [0 5 4 6 7 1 3] (next [0 5 4 6 7 1 3]))
(first (filter (fn[x] < x (next x)) (inits [0 1 4 5 7 3 2 1] )))
