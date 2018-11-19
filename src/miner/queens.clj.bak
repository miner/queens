(ns miner.queens
  "The Eight Queens Puzzle"
  (:require
   [clojure.math.combinatorics :as c]))
    
;; Original idea from blog post:
;; https://feierabendprojekte.wordpress.com/2018/10/27/high-level-vs-low-level/
;; source: https://github.com/Azel4231/muc-codingdojo-queens
;;
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle


;; Dr. Dobbs article from 2005
;; http://www.drdobbs.com/jvm/optimal-queens/184406068


;; ----------------------------------------------------------------------

;; https://rosettacode.org/wiki/N-queens_problem#Clojure


;; SEM changed to 0-based, which is natural for Clojure.
;; Nice short definition with only standard Clojure. Not especially fast but good enough.
(defn rosetta-queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (c/permutations (range n))))


;; ----------------------------------------------------------------------




;; SEM: use one vector of y-coords for queens.  X-coord implied by index.  We know that
;; there can be only one queen in any row.
;;  x-y ([0 1] [1 3] [2 0] [3 2])
;;  ==> [1 3 0 2]
;; use reduce-kv to calc diagonals

;; NOTE: this is still a brute force approach, but it's much better than some other
;; implementations.



;; Note: distinct rows and columns are implied by construction so we don't need to test
;; those.  We make the diagonal markers unique by (- r c dim)

(defn diagonal-safe? [qv]
  (let [dim (count qv)]
    (reduce-kv (fn [diags r c]
                 (let [d1 (+ r c)]
                   (if (diags d1)
                     (reduced false)
                     (let [d2 (- r c dim)]
                       (if (diags d2)
                         (reduced false)
                         (conj! (conj! diags d1) d2))))))
               (transient #{})
               qv)))

(defn brute-queens [dimension]
  (filter diagonal-safe? (c/permutations (range dimension))))

(defn print-board
  ([qv] (print-board (inc (reduce max 5 qv)) qv))
  ([dim qv]
   (println)
   (let [row (vec (take (inc dim) (cycle [" +" " -"])))]
     (dotimes [r dim]
       (let [q (get qv r)
             row (if (even? r) (pop row) (subvec row 1))]
         (println (apply str (if q (assoc row q " Q") row))))))
   (println)))

(defn run-brute-queens
  ([] (run-brute-queens 6))
  ([dim]
   (doseq [qv (brute-queens dim)]
     (print-board qv))
   (println)))





;; converting between representations

;; convert qv vector into row/column coordinates
(defn q2d [qv]
  (map-indexed vector qv))

;; convert row/column coords into vec (row=index, col=val)
;; transient overhead not worth doing since vectors are smallish
(defn q2v [q2s]
  (reduce (fn [r [x y]] (assoc r x y))
          (vec (repeat (count q2s) nil))
          q2s))


(defn abs [n]
  (if (neg? n) (- n) n))


(defn conflict? [v n]
  (let [cnt (count v)]
    (reduce-kv (fn [_ i q]
                 (when (or
                       ;;check for shared row
                       (= q n)
                       ;;check for shared diagonal
                       (= (- cnt i) (abs (- n q))))
                   (reduced true)))
               nil
               v)))

(defn add-queens [vs dimension]
  (for [v vs
        n (range dimension) :when (not (conflict? v n))]
    (conj v n)))

(defn sol5 [dimension]
  (reduce add-queens [[]] (repeat dimension dimension)))




;; proving to myself that this calcs the correct diags.  Tricky but it works as relative
;; offsets get the diagonals in a V pointing left (previous assignments).  Safely ignoring
;; the future direction -- but we have a little issue with twist again.  Should be a V up if
;; rows go down.

(defn rcdiag [r c dim]
  (dotimes [x dim]
    (dotimes [y dim]
      (if (and (= x r) (= y c))
        (print " Q ")
        (if (= (- r x) (abs (- c y)))
          (print " o ")
          (print " . "))))
    (println)))


(defn num-diags [dim]
  (inc (* 2 (dec dim))))

;; Trying to have distinct diag1, diag2 and column numbers so they can live in same set
;; without conflicts.  Simple enough to inline.
(defn diag1 [r c dim]
  (+ r c dim))

(defn diag2 [r c dim]
  (- r c dim))


(defn pdiag1
  ([] (pdiag1 6))
  ([dim] 
   (dotimes [x dim]
     (dotimes [y dim]
       (printf "%3d" (diag1 x y dim)))
     (println))))

(defn pdiag2
  ([] (pdiag2 6))
  ([dim] 
   (dotimes [x dim]
     (dotimes [y dim]
       (printf "%3d" (diag2 x y dim)))
     (println))))


;; But the corners have contention in only one direction so you could skip them for looking
;; for conflicts.  Let's leave that for consideration later.


;; qv [1 3 8 ...]
;; index = row
;; value = col 
;; 
;; diag1 == could put diag1 and diag2 values into sets
;; #{diag1}
;; #{diag2}
;; (diag1 r c dim)
;; (diag1 r c dim)
;;
;; Or guarantee that diag1 and diag2 markers are non-overlapping so they can go into one
;; set.  Just subtract dim from diag2.  All diag2 are now negative, all diag1 are non-neg.

;; store the diag set in the peek position of the vector for convenience


;;; ----------------------------------------------------------------------



;; WAS faster to precompute all the bitcodes and then use them directly, but now we can do
;; it with a function.

;; the extra bit-test on col speeds things up a bit

(defn queens [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (bit-or (bit-shift-left 1 c)
                                  (bit-shift-left 1 (+ r c dimension))
                                  (bit-shift-left 1 (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :when (not (bit-test conflicts col))
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))


;; constant 13 isn't any faster -- dimension seems prettier for offset
(defn queens13 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (bit-or (bit-shift-left 1 c)
                                  (bit-shift-left 1 (+ r c 13))
                                  (bit-shift-left 1 (- r c 13))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :when (not (bit-test conflicts col))
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))





;; bit ops expressions are equivalent but some are a little faster than others
(defn queens2 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (-> (bit-set 0 c)
                           (bit-set (+ r c dimension))
                           (bit-set (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :when (not (bit-test conflicts col))
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))




(defn queens-CACHE [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (vec (for [r (range dimension)
                           c (range dimension)]
                       (-> (bit-set 0 c)
                           (bit-set (+ r c dimension))
                           (bit-set (- r c dimension)))))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :let [rc (rc-code (+ (* row dimension) col))]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    (map pop (reduce add-queens [[0]] (range dimension)))))


(defn queens3 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (vec (for [r (range dimension)
                           c (range dimension)]
                       (-> (bit-shift-left 1 c)
                              (bit-set (+ r c dimension))
                              (bit-set (- r c dimension)))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :let [rc (rc-code (+ (* row dimension) col))]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]

    (map pop (reduce add-queens [[0]] (range dimension)))))

;; slower to do function rc-code.  caching pays off -- but not as nice
(defn rcqueens2 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (-> (bit-set 0 c)
                              (bit-set (+ r c dimension))
                              (bit-set (- r c dimension))))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    (map pop (reduce add-queens [[0]] (range dimension)))))


(defn rcqueens3 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (bit-or (bit-shift-left 1 c)
                                  (bit-shift-left 1 (+ r c dimension))
                                  (bit-shift-left 1 (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    (map pop (reduce add-queens [[0]] (range dimension)))))


;; Turns out to be best of the bit op expressions
(defn rcqueens3a [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (bit-or (bit-shift-left 1 c)
                                  (bit-shift-left 1 (+ r c 13))
                                  (bit-shift-left 1 (- r c 13))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :when (not (bit-test conflicts col))
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))






(defn rcqueens4 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [r c] (-> (bit-shift-left 1 c)
                              (bit-set (+ r c dimension))
                              (bit-set (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           col (range dimension)
                           :let [rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    (map pop (reduce add-queens [[0]] (range dimension)))))




;; only marginally faster to test the col bit first, not worth it.  But in the fn version it
;; is a win so don't be too fast to dismiss
(defn queens1 [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (vec (for [r (range dimension)
                           c (range dimension)]
                       (-> (bit-set 0 c)
                           (bit-set (+ r c dimension))
                           (bit-set (- r c dimension)))))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [state (peek qv)]
                           col (range dimension)
                           :when (not (bit-test state col))
                           :let [rc (rc-code (+ (* row dimension) col))]
                           :when (zero? (bit-and state rc))]
                       (conj (pop qv) col (bit-or state rc))))]
    (map pop (reduce add-queens [[0]] (range dimension)))))



;; SEM idea: do the quick direct bit check separately.  Slightly faster.  I'll take it.


;;; SEM idea: try using meta-data instead of pop vec.  Not better.
;;; SEM other idea -- use constant 13 rather than dim.  Not faster.


;; SEM idea: first row is known to be everything so seed could be changed
;; SEM idea: why precompute?  Do you revisit same node?  Yes as you fan out!
;; not faster to try to preload first pass with all [[i (rc i)] ...]

;; long-array for rc-code only very slightly faster so not worth it.


(defn nqueens [dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [arr-rc (long-array (for [r (range dimension)
                                 c (range dimension)]
                             (-> (bit-set 0 c)
                                 (bit-set (+ r c dimension))
                                 (bit-set (- r c dimension)))))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [state (peek qv)]
                           col (range dimension)
                           :when (not (bit-test state col))
                           :let [rc (aget arr-rc (+ (* row dimension) col))]
                           :when (zero? (bit-and state rc))]
                       (conj (pop qv) col (bit-or state rc))))]
    (mapv pop (reduce add-queens [[0]] (range dimension)))))








(defn run-queens
  ([] (run-queens 6))
  ([dim]
   (doseq [qv (queens dim)]
     (print-board qv))
   (println)))








;; Clever bitwise solution by Martin Richards (2009)
;; Link from Wikipedia article
;; https://www.cl.cam.ac.uk/~mr10/backtrk.pdf

;; Keeps bit patterns for cols
;; LD left diagonal
;; RD right diagonal

;; Possible postions is given by:
;; LET poss = ~(ld | cols | rd) & all
;; poss & -poss yields the least significant one in poss
;; (bit-and poss (- poss)) ==> bit possible queen
;; set that bit and shift the diags out as you step to next level
;; recursive try( (ld|bit)<<1, cols|bit, (rd|bit)>>1 )
;; but he is only counting solutions.  I want to hold onto the bit N in a vector
;; Interesting insight that per row you only have to worry about Dim diags crossing that
;; row, not the totat (* 2 (dec DIM)).  Well, there are still two diags.

;; How fast is it to extract vector of set bits from a long?  Seems like you have to test
;; bits one by one.  Or do the &- trick to get the least bit multiple times.


;; SEM -- no just use the three longs for right-diag left-diag and columns for bookkeeping
;; Keep the vector qv for state
;; Maybe use metadata for the bookkeeping longs or keep them in single vector


(defn bqueens1 [dimension]
  {:pre [(<= 0 dimension 62)]}
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [[rdiag ldiag qcols] (peek qv)]
                           col (range dimension)
                           :when (not (or (bit-test qcols col)
                                          (bit-test rdiag col)
                                          (bit-test ldiag col)))]
                       (conj (pop qv) col [(unsigned-bit-shift-right (bit-set rdiag col) 1)
                                           (bit-and mask (bit-shift-left (bit-set ldiag col)
                                                                         1))
                                           (bit-set qcols col)]))) ]
                                           
    (map pop (reduce add-queens [[[0 0 0]]] (range dimension)))))


;;; slightly faster with
;; :when (zero? (bit-and (bit-or rdiag ldiag qcols) (bit-set 0 col)))
;; and slightly faster again with the shift rather than set
;; :when (zero? (bit-and (bit-or rdiag ldiag qcols) (bit-shift-left 1 col)))
;; but the bit-test is more obvious

;; Note: The Richards solution is smarter about calculating the candidate col from the
;; existing bit patterns.

;;; SEM need to experiment with metadata
(defn bqueensGOOD [dimension]
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)]
                           col (range dimension) :when (not (bit-test (bit-or rdiag ldiag qcols) col))]
                       (with-meta (conj qv col)
                         {::rdiag (unsigned-bit-shift-right (bit-set rdiag col) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-set ldiag col) 1))
                          ::qcols (bit-set qcols col)})))]
    (reduce add-queens [[]] (range dimension))))




(defn bqueensOBVIOUS [dimension]
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)]
                           col (range dimension)
                           :when (not (or (bit-test rdiag col)
                                          (bit-test ldiag col)
                                          (bit-test qcols col)))]
                       (with-meta (conj qv col)
                         {::rdiag (unsigned-bit-shift-right (bit-set rdiag col) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-set ldiag col) 1))
                          ::qcols (bit-set qcols col)})))]
    (reduce add-queens [[]] (range dimension))))



(defn bqueensFASTEST [dimension]
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)]
                           col (range dimension)
                           :when (zero? (bit-and (bit-or rdiag ldiag qcols)
                                                 (bit-shift-left 1 col))) ]
                       (with-meta (conj qv col)
                         {::rdiag (unsigned-bit-shift-right (bit-set rdiag col) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-set ldiag col) 1))
                          ::qcols (bit-set qcols col)})))]
    (reduce add-queens [[]] (range dimension))))





(defn bits [n]
  (loop [bs () n n]
    (if (zero? n)
      bs
      (let [b (Long/numberOfTrailingZeros ^long n)]
        (recur (conj bs b) (bit-clear n b))))))

;; SEM actually could be better if we stay in the bit column rep as we pull out the bits
;; and translate back to the bit index only for the col



;; SEM -- my translation of Martin Richards' algorithm
(defn richards-queensALMOST [dimension]
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)
                                        conflicts (bit-or rdiag ldiag qcols)]
                           col (bits (bit-and mask (bit-not conflicts)))
                           :when (zero? (bit-and conflicts (bit-shift-left 1 col))) ]
                       (with-meta (conj qv col)
                         {::rdiag (unsigned-bit-shift-right (bit-set rdiag col) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-set ldiag col) 1))
                          ::qcols (bit-set qcols col)})))]
    (reduce add-queens [[]] (range dimension))))



(defn bitmasksHigh [n]
  (loop [bs () n n]
    (if (zero? n)
      bs
      (let [b (Long/highestOneBit n)]
        (recur (conj bs b) (bit-xor n b))))))

(defn bitmasks [n]
  (loop [bs () n n]
    (if (zero? n)
      bs
      (let [b (bit-and n (- n))]
        (recur (conj bs b) (bit-xor n b))))))

;;; SEM -- doesn't actually use row in add-queens!!!
(defn ALMOSTrichards-queens [dimension]
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)
                                        conflicts (bit-or rdiag ldiag qcols)]
                           colmask (bitmasksHigh (bit-and-not mask conflicts)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]
    (reduce add-queens [[]] (range dimension))))

;; SAVE but not best
(defn richards-queens000 [dimension]
  (let [mask (dec (bit-set 0 dimension))
        bitmasks (fn [n]
                   (loop [bs () n n]
                     (if (zero? n)
                       bs
                       (let [b (bit-and n (- n))]
                         (recur (conj bs b) (bit-xor n b))))))
        add-queens (fn [qvs]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)
                                        conflicts (bit-or rdiag ldiag qcols)]
                           colmask (bitmasks (bit-and mask (bit-not conflicts))) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]
    (loop [row dimension solutions [[]]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))


(defn calc-qcols [qv]
  (reduce bit-set 0 qv))

(defn calc-ldiag [qv dim]
  (bit-and (dec (bit-shift-left 1 dim))
           (reduce (fn [ldiag q] (bit-shift-left (bit-set ldiag q) 1)) 0 qv)))

(defn calc-rdiag [qv dim]
  (bit-and (dec (bit-shift-left 1 dim))
           (reduce (fn [rdiag q] (unsigned-bit-shift-right (bit-set rdiag q) 1)) 0 qv)))



;; kcalc version are slower
(defn kcalc-rdiag [qv dim]
  (let [cnt (count qv)]
    (reduce-kv (fn [rdiag r c]
                 (let [shift (- c (- cnt r))]
                   (if (neg? shift)
                     rdiag
                     (bit-set rdiag shift))))
               0
               qv)))


(defn kcalc-ldiag [qv dim]
  (let [cnt (count qv)]
    (reduce-kv (fn [ldiag r c]
                 (let [shift (+ c (- cnt r))]
                   (if (< shift dim)
                     (bit-set ldiag shift)
                     ldiag)))
               0
               qv)))





(defn check-diag [qv dim]
  (assert (= (calc-rdiag qv dim) (kcalc-rdiag qv dim)))
  (assert (= (calc-ldiag qv dim) (kcalc-ldiag qv dim)))
  true)


;; Pretty good but not as fast as more bit twiddly rqueens

(defn richards-queens [dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [{::keys [rdiag ldiag qcols]} (meta qv)]
                           colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [(with-meta [] {::rdiag 0 ::ldiag 0 ::qcols 0})]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))

(defn r5-queens [dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [{::keys [rdiag ldiag qcols]
                                         :or {rdiag 0 ldiag 0 qcols 0}} (meta qv)]
                           colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [[]]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))

;; apparently the ::keys destructuring is a bit slower than the obvious way -- maybe this is
;; better even though it's longer???
(defn faster-rich-queens [dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)]
                           colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [[]]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))







(defn ri-queens [dimension]
  (let [mask (dec (bit-set 0 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (or (::rdiag md) (calc-rdiag qv dimension))
                                        ldiag (or (::ldiag md) (calc-ldiag qv dimension))
                                        qcols (or (::qcols md) (calc-qcols qv))
                                        conflicts (bit-or rdiag ldiag qcols)]
                           colmask (open-bitmasks conflicts) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [(with-meta [] {::rdiag 0 ::ldiag 0 ::qcols 0})]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))


;; bit-set is slower than bit-or




;; experimented with single meta-key and [rd ld qc] vector,  slower!


;; sol [qcols rdiag ldiag [qv]]
;; sol (list qv qcols rdiag ldiag) -- not so fast



;; Surprisingly fast destructuring against vector
;; list version was not so good
;; SAVE THIS ONE
(defn svqueens [dimension]
  (let [mask (dec (bit-set 0 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qvx qvs :let [[qv qcols rdiag ldiag] qvx
                                         conflicts (bit-or rdiag ldiag qcols)]
                           colmask (open-bitmasks conflicts) ]
                       (vector (conj qv (Long/numberOfTrailingZeros colmask))
                             (bit-or qcols colmask)
                             (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                             (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))))) ]

    (loop [row dimension solutions [[[] 0 0 0]] ]
      (if (zero? row)
        (map first solutions)
        (recur (dec row) (add-queens solutions))))))


;; super poppy not so good
(defn spopqueens [dimension]
  (let [mask (dec (bit-set 0 dimension))
        
        open-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qvx qvs :let [ldiag (peek qvx)
                                         rdiag (peek (pop qvx))
                                         qcols (peek (pop (pop qvx)))
                                         qv (pop (pop (pop qvx)))
                                         conflicts (bit-or rdiag ldiag qcols)]
                           colmask (open-bitmasks conflicts) ]
                       (conj qv
                             (Long/numberOfTrailingZeros colmask)
                             (bit-or qcols colmask)
                             (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                             (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1)) ))) ]

    (loop [row dimension solutions [[0 0 0]] ]
      (if (zero? row)
        (map #(pop (pop (pop %))) solutions)
        (recur (dec row) (add-queens solutions))))))




;;  Loop is faster than reduce for this case where row wasn't being used anyway.



;; SEM: cheating a bit to limit to 14 bits so the high bit of the 16-bit field is always zero and
;; never shifts into another field.  Save a couple of bit-ands.   BUT IT WENT SLOWER????

(defn rqueens-FASTEST [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask (bit-or conflicts
                                                                 (unsigned-bit-shift-right conflicts 16)
                                                                 (unsigned-bit-shift-right conflicts 32)))]
                          (if (zero? n)
                            bs
                            (let [b (Long/highestOneBit n)]
                              (recur (conj bs (bit-or (bit-shift-left b 32)
                                                      (bit-shift-left b 16) b))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask 16) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask 32)
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))



(defn rqueens-CLEANER [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask (bit-or conflicts
                                                   (unsigned-bit-shift-right conflicts 16)
                                                   (unsigned-bit-shift-right conflicts 32))) ]
                          (if (zero? n)
                            bs
                            (let [b (Long/highestOneBit n)]
                              (recur (conj bs (bit-or (bit-shift-left b 32)
                                                      (bit-shift-left b 16) b))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask 16) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask 32)
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))

;; using lowestOneBit is slightly faster than highestOneBit  -- produces solutions in
;; different order but same set.
;; lowestOneBit is same as (bit-and n (- n))



;;; SEM new idea, unimplemented.  Keep diags in same word so you can shift together.  Have
;;; to have (- Dimension col) to check backwards diag  but maybe cheaper than extra bit
;;; shift???
;;;  Need to mask shifted high but can use one spare bit separator and the main mask can be
;;; combined to clear the spare bit.  Two 32 bits are plenty of space.  Never go above
;;; DIM=31
;; or...   clear bit 31 before the rotate left, then you don't need the spare bit



;; Currently FASTEST actually, but the word packing into one long doesn't seem tidy
(defn fastest-queens [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not
                                        mask
                                        (bit-or conflicts
                                                (unsigned-bit-shift-right conflicts 16)
                                                (unsigned-bit-shift-right conflicts 32))) ]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (bit-or (bit-shift-left b 32)
                                                      (bit-shift-left b 16) b))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask 16) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask 32)
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




(defn rqueens [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not
                                        mask
                                        (bit-or conflicts
                                                (unsigned-bit-shift-right conflicts 16)
                                                (unsigned-bit-shift-right conflicts 32))) ]
                          (if (zero? n)
                            bs
                            (let [b (bit-and n (- n))]
                              (recur (conj bs (bit-or (bit-shift-left b 32)
                                                      (bit-shift-left b 16) b))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask 16) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask 32)
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




;;; SEM use bit-and-not


;;; SEM idea: qcols at 48, ldiag 32, rdiag 16, total conflicts at 0
;;; Not faster, but more code so not good.
(defn r2queens [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (bit-or b (bit-shift-left b 16)
                                                      (bit-shift-left b 32)
                                                      (bit-shift-left b 48)))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)
                                rdiag (bit-and mask16 (unsigned-bit-shift-right flicts 1))
                                ldiag (bit-and mask32 (bit-shift-left flicts 1))
                                qcols (bit-and mask48 flicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




(defn rdqueens [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and-not
                                        mask
                                        (bit-or conflicts
                                                (unsigned-bit-shift-right conflicts 16)
                                                (unsigned-bit-shift-right conflicts 32))) ]
                          (if (zero? n)
                            bs
                            (let [b (bit-and n (- n))]
                              (recur (conj bs (bit-or (bit-shift-left b 32)
                                                      (bit-shift-left b 16) b))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask 16) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask 32)
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))






(defn rqueens-SLOWER [dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [conflicts]
                        (loop [bs () n (bit-and mask
                                                (bit-not (bit-or conflicts
                                                                 (unsigned-bit-shift-right
                                                                  conflicts (inc dimension))
                                                                 (unsigned-bit-shift-right
                                                                  conflicts (* 2 (inc
                                                                                  dimension))))))]
                          
                          (if (zero? n)
                            bs
                            (let [b (Long/highestOneBit n)]
                              (recur (conj bs (bit-or b
                                                      (bit-shift-left b (inc dimension))
                                                      (bit-shift-left b (* 2 (inc dimension)))))
                                     (bit-xor n b))))))

        shift-conflicts (fn [conflicts colmask]
                          (let [flicts (bit-or conflicts colmask)]
                            (bit-or (bit-and mask flicts)
                                    (bit-and (bit-shift-left mask (inc dimension)) 
                                             (unsigned-bit-shift-right flicts 1))
                                    (bit-and (bit-shift-left mask (* 2 (inc dimension)))
                                             (bit-shift-left flicts 1)))))
        
        add-queens (fn [qvs row]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts conflicts colmask)))) ]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))



;; slower
(defn bqueens2 [dimension]
  {:pre [(<= 0 dimension 62)]}
  (let [mask (dec (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [[rdiag ldiag qcols] (peek qv)]
                           col (range dimension)
                           :when (not (bit-test (bit-or rdiag ldiag qcols) col))]
                       (conj (pop qv) col [(bit-shift-right (bit-set rdiag col) 1)
                                           (bit-and mask (bit-shift-left (bit-set ldiag col)
                                                                         1))
                                           (bit-set qcols col)]))) ]
                                           
    (map pop (reduce add-queens [[[0 0 0]]] (range dimension)))))




;; copied from builtin distinct?
(defn distin?
  "Returns true if no two of the arguments are ="
  {:tag Boolean
   :added "1.0"
   :static true}
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not= x y)
     (loop [s #{x y} [x & etc :as xs] more]
       (if xs
         (if (contains? s x)
           false
           (recur (conj s x) etc))
         true))
     false)))


(defn dist?
  "Returns true if no two of the arguments are ="
  {:tag Boolean
   :added "1.0"
   :static true}
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not= x y)
     (loop [s (transient #{x y}) [x & etc :as xs] more]
       (if xs
         (if (contains? s x)
           false
           (recur (conj! s x) etc))
         true))
     false)))


(defn rdist? [coll]
  (boolean (reduce (fn [seen x] (if (contains? seen x) (reduced false) (conj! seen x)))
                   (transient #{})
                   coll)))

(defn ldist1? [coll]
  (loop [s (transient #{}) [x & etc :as xs] coll]
    (if xs
      (if (contains? s x)
        false
        (recur (conj! s x) etc))
      true)))


(defn ldist? [coll]
  (loop [s (transient #{}) xs (seq coll)]
    (if xs
      (if (contains? s (first xs))
        false
        (recur (conj! s (first xs)) (next xs)))
      true)))



(require '[criterium.core :as cc])
(require '[clojure.string :as str])

(defn fname [f]
  (let [stfn (str f)
        at (str/index-of stfn "@")]
    (if at
      (str/replace (subs stfn (count "miner.queens$") at) \_ \-)
      stfn)))

(defn ben [& fs]
  (let [dim 8
        answer (count (queens dim))]
    (doseq [f fs]
      (println)
      (println (fname f))
      (cc/quick-bench (assert (= answer (count (f dim))))))))

