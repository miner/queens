(ns miner.queens
  "The Eight Queens Puzzle"
  (:require
   [clojure.math.combinatorics :as c]))


;; slightly faster numeric code with unchecked-math
(set! *unchecked-math* :warn-on-boxed)

;; ----------------------------------------------------------------------
;;
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle
;;
;; Dr. Dobbs article from 2005
;; http://www.drdobbs.com/jvm/optimal-queens/184406068


;; https://rosettacode.org/wiki/N-queens_problem#Clojure
;; SEM changed to 0-based, which is natural for Clojure.  Nice, short definition with
;; standard Clojure, plus a contrib library.  Not especially fast but good enough.

(defn rosetta-queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (c/permutations (range n))))


;; somewhat faster than (c/permuations (range n))
(defn range-permutations
  "Returns an eager sequence of vectors representing the permutations of the half-open
  range [0, N)."
  [n]
  {:pre [(not (neg? ^long n))]}
  (reduce (fn [vs cnt]
            (reduce (fn [acc vvv]
                      (reduce-kv (fn [r i x] (conj r (assoc vvv i cnt cnt x)))
                                 (conj acc (conj vvv cnt))
                                 vvv))
                    ()
                    vs))
          (list [])
          (range n)))


(defn my-rosetta-queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (range-permutations n)))



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




(defn queens [^long dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [rc-code (fn [^long r ^long c] (bit-or (bit-shift-left 1 c)
                                              (bit-shift-left 1 (+ r c dimension))
                                              (bit-shift-left 1 (- r c dimension))))

        add-queens (fn [qvs row]
                     (for [qv qvs :let [^long conflicts (peek qv)]
                           col (range dimension)
                           :let [^long rc (rc-code row col)]
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

;; for CLJS, we might need:


;; Long/lowestOneBit
(defn lowOneBit [^long n]
  (bit-and n (- n)))

;; almost as fast as Long/numberOfTrailingZeros
(defn numTrailingZeros [^long n]
  (cond (zero? n) 64
        (bit-test n 0) 0
        :else   (loop [v n c 0 p 32]
                  (if (zero? p)
                    c
                    (if (zero? (bit-and v (dec (bit-shift-left 1 p))))
                      (recur (unsigned-bit-shift-right v p) (+ c p) (quot p 2))
                      (recur v c (quot p 2)))))))


(defn test-num-trail [f]
  (assert (every? #(= (f %) (Long/numberOfTrailingZeros %))
                  (concat (take 100 (iterate inc Long/MIN_VALUE))
                          (take 100 (iterate dec Long/MAX_VALUE))
                          (take 100 (iterate inc Integer/MIN_VALUE))
                          (take 100 (iterate dec Integer/MAX_VALUE))
                          (range -1000 1000))))
  true)





;;; SEM new idea:  Keep diags in same word so you can shift together.  Have
;;; to have (- Dimension col) to check backwards diag  but maybe cheaper than extra bit
;;; shift???
;;;  Need to mask shifted high but can use one spare bit separator and the main mask can be
;;; combined to clear the spare bit.  Two 32 bits are plenty of space.  Never go above
;;; DIM=31
;; or...   clear bit 31 before the rotate left, then you don't need the spare bit


;;; SEM use bit-and-not


;;; SEM idea: qcols at 48, ldiag 32, rdiag 16, total conflicts at 0
;;;   not that limit of 14 bit fields saves a 0 bit buffer between fields for safe shifting
;;;
;;; Slightly faster, but more code so not sure

;; hints improve (q 8) - 860 vs 880ms

(defn almost-fastest-queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        free-bitmasks (fn [^long conflicts]
                        (loop [bs () n (bit-and-not mask conflicts)]
                          (if (zero? n)
                            bs
                            (let [colmask (bit-shift-left 0x0001000100010001
                                                          (Long/numberOfTrailingZeros n))]
                              (recur (conj bs colmask)
                                     (bit-and-not n colmask))))))

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




;;; loops without consing definitely pays off for speed if not readability.

(defn fastest-queens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)

        shift-conflicts (fn [^long conflicts]
                          (let [rdiag (bit-and mask16 (unsigned-bit-shift-right conflicts 1))
                                ldiag (bit-and mask32 (bit-shift-left conflicts 1))
                                qcols (bit-and mask48 conflicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens
        (fn [qvs]
          (loop [qvs qvs xvs ()]
            (if-let [qv (first qvs)]
              (recur (rest qvs)
                     (let [^long conflicts (peek qv)]
                       (loop [vs xvs n (bit-and-not mask conflicts)]
                         (if (zero? n)
                           vs
                           (let [q (Long/numberOfTrailingZeros n)
                                 colmask (bit-shift-left 0x0001000100010001 q)]
                             (recur (conj vs (conj (pop qv) q
                                                   (shift-conflicts (bit-or conflicts colmask))))
                                    (bit-and-not n colmask)))))))
              
              xvs))) ]

    (loop [x dimension solutions (list [0])]
      (if (zero? x)
        (map pop solutions)
        (recur (dec x) (add-queens solutions))))))




(set! *unchecked-math* false)

;;; SEM new idea -- start with all singles [0] [1], etc [N] and run the rest in parallel



(require '[criterium.core :as cc])
(require '[clojure.string :as str])

(defn fname [f]
  (let [stfn (str f)
        at (str/index-of stfn "@")]
    (if at
      (str/replace (subs stfn (count "miner.queens$") at) \_ \-)
      stfn)))

(defn bench [& fs]
  (let [dim 8
        answer (count (queens dim))]
    (doseq [f fs]
      (println)
      (println (fname f))
      (cc/quick-bench (assert (= answer (count (f dim))))))))


(defn bentrz [f]
  (criterium.core/quick-bench (test-num-trail f)))

