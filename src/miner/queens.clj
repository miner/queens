(ns miner.queens
  "The Eight Queens Puzzle"
  (:require
   [clojure.math.combinatorics :as c]))

;;
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle
;;
;; Dr. Dobbs article from 2005
;; http://www.drdobbs.com/jvm/optimal-queens/184406068


;; ----------------------------------------------------------------------

;; https://rosettacode.org/wiki/N-queens_problem#Clojure


;; SEM changed to 0-based, which is natural for Clojure.
;; Nice, short definition with only standard Clojure. Not especially fast but good enough.

(set! *unchecked-math* :warn-on-boxed)


;; somewhat faster than (c/permuations (range n))
(defn range-permutations
  "Returns an eager sequence of vectors representing the permutations of the half-open
  range [0, N)."
  [^long n]
  {:pre [(not (neg? n))]}
  (reduce (fn [vs cnt]
            (reduce (fn [acc vvv]
                      (reduce-kv (fn [r i x] (conj r (assoc vvv i cnt cnt x)))
                                 (conj acc (conj vvv cnt))
                                 vvv))
                    ()
                    vs))
          (list [])
          (range n)))


(defn rosetta-queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (range-permutations n)))



(defn ros-queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (c/permutations (range n))))


;; ----------------------------------------------------------------------


;;; SEM need to try pmap and reducers -- didn't help.  Too much overhead for a short driving
;;; sequence.

;;; SEM new idea: instead of single diagonals, look for a V-shaped (abs) marker from each
;;; position.  Can you make markers for that?  Doesn't seem like an advantage.



;;; SEM -- try a deftype for Richards
;;;  maybe even for your queens instead of bitwise

;; (deftype Richards
;;     [^long mask
;;      ^long qcols
;;      ^long ldiag
;;      ^long rdiag
;;      ^long conflicts]
;; 
;;   RichardsOps
;;   (add-queen [this col]
;;     (when-not (zero? conflicts)
;;       (let [open-bits (bit-and-not mask conflicts)
;; 
;; 
;; (defn empty-richards [mask]
;;   (Richards. mask 0 0 0 0)))
;; 
;; (defn add-queen [rich col] ...)
;; do shifts and merge conflicts
             

;; SEM new idea : don't seq open-bits
;;  just recur on the lowest bit
;;  if none -- go on





;; SEM: use one vector of y-coords for queens.  X-coord implied by index.  We know that
;; there can be only one queen in any row.
;;  x-y ([0 1] [1 3] [2 0] [3 2])
;;  ==> [1 3 0 2]
;; use reduce-kv to calc diagonals




(defn print-board
  ([qv] (print-board (inc ^long (reduce max 5 qv)) qv))
  ([^long dim qv]
   (println)
   (let [row (vec (take (inc dim) (cycle [" +" " -"])))]
     (dotimes [r dim]
       (let [q (get qv r)
             row (if (even? r) (pop row) (subvec row 1))]
         (println (apply str (if q (assoc row q " Q") row))))))
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


(defn abs [^long n]
  (if (neg? n) (- n) n))


(defn conflict? [v ^long n]
  (let [cnt (count v)]
    (reduce-kv (fn [_ ^long i ^long q]
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

(defn queens5 [dimension]
  (reduce add-queens [[]] (repeat dimension dimension)))



(defn pdiag1
  ([] (pdiag1 6))
  ([^long dim] 
   (dotimes [x dim]
     (dotimes [y dim]
       (printf "%3d" (- x y dim)))
     (println))))

(defn pdiag2
  ([] (pdiag2 6))
  ([^long dim] 
   (dotimes [x dim]
     (dotimes [y dim]
       (printf "%3d" (+ x y dim)))
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

(defn queens [^long dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [^long r ^long c] (bit-or (bit-shift-left 1 c)
                                              (bit-shift-left 1 (+ r c dimension))
                                              (bit-shift-left 1 (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [^long conflicts (peek qv)]
                           col (range dimension)
                           :when (not (bit-test conflicts col))
                           :let [^long rc (rc-code row col)]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))

#_
(defn no-hint-queens [dimension]
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




;;; slightly faster with
;; :when (zero? (bit-and (bit-or rdiag ldiag qcols) (bit-set 0 col)))
;; and slightly faster again with the shift rather than set
;; :when (zero? (bit-and (bit-or rdiag ldiag qcols) (bit-shift-left 1 col)))
;; but the bit-test is more obvious

;; Note: The Richards solution is smarter about calculating the candidate col from the
;; existing bit patterns.


;; slightly faster than richards-queens but doesn't do clever bit on colmask
(defn bqueens [^long dimension]
  (let [mask (dec ^long (bit-set 0 dimension))
        add-queens (fn [qvs row]
                     (for [qv qvs :let [md (meta qv)
                                        rdiag (::rdiag md 0)
                                        ldiag (::ldiag md 0)
                                        qcols (::qcols md 0)]
                           ^long col (range dimension)
                           :when (zero? (bit-and (bit-or ^long rdiag ^long ldiag ^long qcols)
                                                 (bit-shift-left 1 col))) ]
                       (with-meta (conj qv col)
                         {::rdiag (unsigned-bit-shift-right ^long (bit-set rdiag col) 1)
                          ::ldiag (bit-and mask (bit-shift-left ^long (bit-set ldiag col) 1))
                          ::qcols (bit-set qcols col)})))]
    (reduce add-queens [[]] (range dimension))))





(defn calc-qcols [qv]
  (reduce bit-set 0 qv))

(defn calc-ldiag [qv ^long dim]
  (bit-and (dec (bit-shift-left 1 dim))
           ^long (reduce (fn [^long ldiag ^long q] (bit-shift-left ^long (bit-set ldiag q) 1)) 0 qv)))

(defn calc-rdiag [qv ^long dim]
  (bit-and (dec (bit-shift-left 1 dim))
           ^long (reduce (fn [rdiag q] (unsigned-bit-shift-right ^long (bit-set rdiag q) 1)) 0 qv)))



;; Pretty good but not as fast as more bit twiddly rqueens

;; slightly faster to use explicit key destructing rather than cool ::keys
(defn richards-queens [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [m (meta qv)
                                        ^long rdiag (::rdiag m)
                                        ^long ldiag (::ldiag m)
                                        ^long qcols (::qcols m)]
                           ^long colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [(with-meta [] {::rdiag 0 ::ldiag 0 ::qcols 0})]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))







(defn richards-queens-keys [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [{::keys [rdiag ldiag qcols]} (meta qv)]
                           colmask (open-bitmasks (bit-or ^long rdiag ^long ldiag ^long qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right ^long (bit-or ^long rdiag ^long colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ^long ldiag ^long colmask) 1))
                          ::qcols (bit-or ^long qcols ^long colmask)})))]

    (loop [row dimension solutions [(with-meta [] {::rdiag 0 ::ldiag 0 ::qcols 0})]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))




#_
(defn r5-queens [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [{::keys [^long rdiag ^long ldiag ^long qcols]
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


;; NO, none of this list messing is any faster
(defn zqueens [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [qcols (first qv)
                                        ldiag (second qv)
                                        rdiag (first (nnext qv))
                                        qs (drop 3 qv)
                                        conflicts (bit-or ^long rdiag ^long ldiag ^long qcols)] 
                           col (range dimension) :when (not (bit-test conflicts col))]
                       (conj qs
                             col
                             (unsigned-bit-shift-right ^long (bit-set rdiag col) 1)
                             (bit-and mask (bit-shift-left ^long (bit-set ldiag col) 1))
                             (bit-set qcols col)))) ]

    (loop [row dimension solutions ['(0 0 0)] ]
      (if (zero? row)
        (map #(drop 3 %) solutions)
        (recur (dec row) (add-queens solutions))))))




;; apparently the ::keys destructuring is a bit slower than the obvious way -- maybe this is
;; better even though it's longer???
(defn faster-rich-queens [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
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
                           colmask (open-bitmasks (bit-or ^long rdiag ^long ldiag ^long qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right ^long (bit-or ^long rdiag ^long colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left ^long (bit-or ^long ldiag
                     ^long colmask) 1))
                          ::qcols (bit-or ^long qcols ^long colmask)})))]

    (loop [row dimension solutions [[]]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))




;; somewhat faster richards, but not always???
(defn rich-queens2 [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [md (meta qv)
                                        ^long rdiag (::rdiag md 0)
                                        ^long ldiag (::ldiag md 0)
                                        ^long qcols (::qcols md 0)]
                           ^long colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [[]]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))



;; why isn't this faster than rich-queens2 ???
(defn rich-queens3 [^long dimension]
  (let [mask (dec (bit-shift-left 1 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [md (meta qv)
                                        ^long rdiag (::rdiag md)
                                        ^long ldiag (::ldiag md)
                                        ^long qcols (::qcols md)]
                           ^long colmask (open-bitmasks (bit-or rdiag ldiag qcols)) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or rdiag colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ldiag colmask) 1))
                          ::qcols (bit-or qcols colmask)})))]

    (loop [row dimension solutions [(with-meta [] {::rdiag 0 ::ldiag 0 ::qcols 0})]]
      (if (zero? row)
        solutions
        (recur (dec row) (add-queens solutions))))))












(defn ri-queens [^long dimension]
  (let [mask (dec ^long (bit-set 0 dimension))
        
        open-bitmasks (fn [^long conflicts]
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
                                        conflicts (bit-or ^long rdiag ^long ldiag ^long qcols)]
                           colmask (open-bitmasks conflicts) ]
                       (with-meta (conj qv (Long/numberOfTrailingZeros colmask))
                         {::rdiag (unsigned-bit-shift-right (bit-or ^long rdiag ^long colmask) 1)
                          ::ldiag (bit-and mask (bit-shift-left (bit-or ^long ldiag ^long colmask) 1))
                          ::qcols (bit-or ^long qcols ^long colmask)})))]

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
(defn svqueens [^long dimension]
  (let [mask (dec ^long (bit-set 0 dimension))
        
        open-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b) (bit-xor n b))))))
        
        add-queens (fn [qvs]
                     (for [qvx qvs :let [[qv qcols rdiag ldiag] qvx
                                         conflicts (bit-or ^long rdiag ^long ldiag ^long qcols)]
                           colmask (open-bitmasks conflicts) ]
                       (vector (conj qv (Long/numberOfTrailingZeros colmask))
                             (bit-or ^long qcols ^long colmask)
                             (unsigned-bit-shift-right (bit-or ^long rdiag ^long colmask) 1)
                             (bit-and mask (bit-shift-left (bit-or ^long ldiag ^long colmask) 1))))) ]

    (loop [row dimension solutions [[[] 0 0 0]] ]
      (if (zero? row)
        (map first solutions)
        (recur (dec row) (add-queens solutions))))))


;;  Loop is faster than reduce for this case where row wasn't being used anyway.



;; SEM: cheating a bit to limit to 14 bits so the high bit of the 16-bit field is always zero and
;; never shifts into another field.  Save a couple of bit-ands.   BUT IT WENT SLOWER????


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



;; Currently SECOND FASTEST actually, but the word packing into one long doesn't seem tidy
(defn second-fastest-queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [^long conflicts]
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

        shift-conflicts (fn [^long conflicts ^long colmask]
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




(defn rqueens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [^long conflicts]
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

        shift-conflicts (fn [^long conflicts ^long colmask]
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
;;;   not that limit of 14 bit fields saves a 0 bit buffer between fields for safe shifting
;;;
;;; Slightly faster, but more code so not sure

;; hints improve (q 8) - 860 vs 880ms

;; ACTUALLY -- currently fastest
(defn fastest-queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (bit-or b (bit-shift-left b 16)
                                                      (bit-shift-left b 32)
                                                      (bit-shift-left b 48)))
                                     (bit-xor n b))))))

        shift-conflicts (fn [^long conflicts]
                          (let [rdiag (bit-and mask16 (unsigned-bit-shift-right conflicts 1))
                                ldiag (bit-and mask32 (bit-shift-left conflicts 1))
                                qcols (bit-and mask48 conflicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [^long conflicts (peek qv)]
                           ^long colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts (bit-or conflicts colmask))))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




#_
(defn without-hints-queens [dimension]
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

        shift-conflicts (fn [conflicts]
                          (let [rdiag (bit-and mask16 (unsigned-bit-shift-right conflicts 1))
                                ldiag (bit-and mask32 (bit-shift-left conflicts 1))
                                qcols (bit-and mask48 conflicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts (bit-or conflicts colmask))))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))






(defn f8queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (bit-or b (bit-shift-left b 16)
                                                      (bit-shift-left b 32)
                                                      (bit-shift-left b 48)))
                                     (bit-xor n b))))))

        shift-conflicts (fn [^long conflicts]
                          (let [rdiag (bit-and mask16 (unsigned-bit-shift-right conflicts 1))
                                ldiag (bit-and mask32 (bit-shift-left conflicts 1))
                                qcols (bit-and mask48 conflicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens (fn [qvs]
                     (for [qv qvs :let [conflicts (peek qv)]
                           colmask (free-bitmasks conflicts)]
                       (conj (pop qv)
                             (Long/numberOfTrailingZeros colmask)
                             (shift-conflicts (bit-or ^long conflicts ^long colmask))))) ]

    (loop [x dimension solutions [[0]]]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




(defn f4queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (bit-or b (bit-shift-left b 16)
                                                      (bit-shift-left b 32)
                                                      (bit-shift-left b 48)))
                                     (bit-xor n b))))))

        shift-conflicts (fn [^long conflicts ^long colmask]
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




(defn f3queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs (reduce (fn [^long bits ^long n] (bit-or bits
                                                                           (bit-shift-left
                                                                            b n)))
                                                      b
                                                      [16 32 48]))
                                     (bit-xor n b))))))

        shift-conflicts (fn [^long conflicts ^long colmask]
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




;; Not faster
(defn f2queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [b (Long/lowestOneBit n)]
                              (recur (conj bs b)
                                     (bit-xor n b))))))

        shift-conflicts (fn [^long conflicts ^long colmask]
                          ;; single bit in colmask
                          (let [rdiag (bit-and mask (bit-or (unsigned-bit-shift-right colmask 1)
                                                            (unsigned-bit-shift-right
                                                             conflicts 17)))
                                ldiag (bit-and mask (bit-or (bit-shift-left colmask 1)
                                                            (unsigned-bit-shift-right
                                                             conflicts 31)))
                                qcols (bit-or colmask (unsigned-bit-shift-right
                                                       conflicts 48)) ]
                            (bit-or rdiag ldiag qcols
                                    (bit-shift-left rdiag 16)
                                    (bit-shift-left ldiag 32)
                                    (bit-shift-left qcols 48))))
        
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



(defn rdqueens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        free-bitmasks (fn [^long conflicts]
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

        shift-conflicts (fn [^long conflicts ^long colmask]
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



;;; SEM new idea -- start with all singles [0] [1], etc [N] and run the rest in parallel


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


(set! *unchecked-math* false)
