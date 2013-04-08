(ns fourclojure.core-test
  (:use clojure.test
        fourclojure.core))

(deftest p-152
  (testing "Latin Square Slicing"
    (is (= {4 1, 3 1, 2 7}
(
(let [

latin-square? (fn latin-square? [square]
  (case (count square)
    2  (let [[[a b] [c d]] square]
      (and
        (not= a b)
        (= a d)
        (= b c)))
    3  (let [[[a b c] [d e f] [g h i]] square]
      (and
        (not= a b)
        (not= b c)
        (not= a c)
        (or
          (and 
            (= [b c a] [d e f])
            (= [c a b] [g h i]))
          (and
            (= [c a b] [d e f])
            (= [b c a] [g h i])))))
  (let [n          (count square)
        n-unique?  #(= n (count (set %)))]
    (and
      (every? n-unique? square)
      (n-unique? (flatten square))
      (every? n-unique? (apply map vector square))))))

;; Initially n vectors, constraints = [0 0], acc = []
get-squares-on-line (fn get-squares-on-line [vectors n max-n cl cr acc]
  (if (empty? vectors)
    (if (latin-square? acc) [acc] [])
    (let [line           (first vectors)
          squares-seq    (map vec (partition n 1 line))
          left-seq       (map #(max % cl) (range))
          right-seq      (map #(max % cr) (iterate dec (- (count line) n)))
          next-calls     (map #(vector % %2 %3) squares-seq left-seq right-seq)
          should-call?   (fn [[v nl nr]] (<= (+ n nl nr) max-n))
          filter-calls   (filter should-call? next-calls)]
      (mapcat (fn [[v nl nr]]
                (get-squares-on-line (rest vectors) n max-n nl nr (conj acc v)))
              filter-calls))))

get-squares-n (fn get-squares-n [vectors n]
  (let [max-n (reduce max (map count vectors))]
    (mapcat #(get-squares-on-line % n max-n 0 0 []) (partition n 1 vectors))))

get-squares (fn get-squares [vectors]
  (let [max-sq (min (count vectors) (reduce max (map count vectors)))]
    (mapcat #(get-squares-n vectors %) (range 2 (inc max-sq)))))]

(fn find-all [vectors]
  (time (reduce
    (fn [p x] (update-in p [(count x)] (fnil inc 0)))
    {}
    (set (get-squares vectors))))))

  [[8 6 7 3 2 5 1 4]
   [6 8 3 7]
   [7 3 8 6]
   [3 7 6 8 1 4 5 2]
   [1 8 5 2 4]
   [8 1 2 4 5]])))))

(println (take 10 (

(let [
  expand-state (fn expand-state [dfa state prefix]
    (map #(vector (second %) (conj prefix (first %)))(seq (get-in dfa [:transitions state]))))

  gen-strs (fn gen-strs [dfa q]
    (if (empty? q)
      []
      (let [[state prefix] (peek q)
            new-rest  (lazy-seq (gen-strs dfa (into (pop q) (expand-state dfa state prefix))))]
        (if (get-in dfa [:accepts state])
          (cons prefix new-rest)
          new-rest))))]

(fn [dfa]
  (map
   #(apply str %)
   (gen-strs
    dfa
    (conj (clojure.lang.PersistentQueue/EMPTY) [(get-in dfa [:start]) []] )))))

;;;
;;'{:states #{q0}
;;  :alphabet #{0}
;;  :start q0
;;  :accepts #{q0}
;;  :transitions {q0 {0 q0}}})
'{:states #{q0 q1}
  :alphabet #{0 1}
  :start q0
  :accepts #{q0}
  :transitions {q0 {0 q0, 1 q1}
                q1 {0 q1, 1 q0}}})

;;'{:states #{q0 q1 q2 q3}
;;  :alphabet #{a b c}
;;  :start q0
;;  :accepts #{q1 q2 q3}
;;  :transitions {q0 {a q1}
;;                q1 {b q2}
;;                q2 {c q3}}})

))
