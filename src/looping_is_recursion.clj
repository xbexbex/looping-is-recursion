(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [acc n]
                   (if (= 1 n)
                     acc
                     (recur (* base acc) (dec n))))]
      (helper base exp))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [a-seq]
                   (if (= 1 (count a-seq))
                     (first a-seq)
                     (recur (rest a-seq))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq))
        true
      (not (= (count a-seq) (count b-seq)))
        false
      (= (first a-seq) (first b-seq))
        (recur (rest a-seq) (rest b-seq))
      :else
        false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [z 0]
    (cond (= z (count a-seq))
            nil
          (pred (get a-seq z))
            z
          :else
        (recur (inc z)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (let [n (count a-seq)]
      (loop [i 0
             s 0]
        (if (= i n)
          (/ s n)
          (recur (inc i) (+ s (get a-seq i))))))))

(defn parity [a-seq]
  (if (empty? a-seq)
    nil
    (let [toggle (fn [a-set elem]
                    (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
      (loop [a #{}
             b-seq a-seq]
        (if (empty? b-seq)
          a
          (recur (toggle a (first b-seq)) (rest b-seq)))))))

(defn fast-fibo [n]
  (cond (= n 0)
        0
        (= n 1)
        1
        :else
          (loop [i 2
                 n1 1
                 n2 0]
            (if (= n i)
              (+ n1 n2)
              (recur (inc i) (+ n1 n2) n1)))))

(defn cut-at-repetition [a-seq]
  (loop [a []
         c #{}
         b a-seq]
    (cond (empty? b)
            a
          (contains? c (first b))
            a
          :else
            (recur (conj a (first b)) (conj c (first b)) (rest b)))))
