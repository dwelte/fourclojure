(ns fourclojure.core-test
  (:use clojure.test
        fourclojure.core))

(deftest p-152
  (testing "Latin Square Slicing"
    (is (= {4 1, 3 1, 2 7}
(
(let [

get-vector-alignments (fn get-vector-alignments [v max-length]
  (if (empty? v)
    [(vec (repeat max-length nil))]
    (let [nils-count (- max-length (count v))]
      (map 
        #(vec
          (concat
            (repeat % nil)
            v
            (repeat (- nils-count %) nil)))
        (range (inc nils-count))))))

get-arrangements (fn get-arrangements
  ([vectors] (get-arrangements vectors (reduce max (map count vectors))))
  ([vectors max-length]
    (if (empty? vectors)
    '([])
    (for [vector-bottoms (get-arrangements (rest vectors) max-length)
          vector-top (get-vector-alignments (first vectors) max-length)]
      (into [vector-top] vector-bottoms)))))

get-x (fn get-x [rectangle]
  (count (first rectangle))) 

get-y (fn get-y [rectangle]
  (count rectangle)) 

get-min-dim (fn get-min-dim [rectangle]
  (min (count (first rectangle)) (count rectangle)))

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

get-square (fn get-square [rectangle x y n]
  (let [rows (subvec rectangle y (+ n y))]
    (if (every? #(and (% x) (dec (+ x n))) rows)
      (let [square (vec (map #(subvec % x (+ n x)) rows))]
        (if (latin-square? square) 
          square
          nil)))))
      
get-squares (fn get-squares [rectangle]
  (let [upper-range (inc (get-min-dim rectangle))]
    (for [n (range 2 upper-range)
          x (range (inc (- (get-x rectangle) n)))
          y (range (inc (- (get-y rectangle) n)))
          :let [square (get-square rectangle x y n)]
          :when square] 
      square)))]

(fn find-all [vectors]
  (time (reduce
    (fn [p x] (update-in p [(count x)] (fnil inc 0)))
    {}
    (set (mapcat get-squares (get-arrangements vectors)))))))

  [[8 6 7 3 2 5 1 4]
   [6 8 3 7]
   [7 3 8 6]
   [3 7 6 8 1 4 5 2]
   [1 8 5 2 4]
   [8 1 2 4 5]])))))
