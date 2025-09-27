(ns miner.queens
  "The Eight Queens Puzzle"
  (:require
   [clojure.math.combinatorics :as c]
   [criterium.core :as cc]
   [clojure.string :as str]))


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
  (let [add-queens (fn [qvs ^long row]
                     (for [qv qvs :let [conflicts (long (peek qv))]
                           col (range dimension)
                           :let [col (long col)
                                 rc (bit-or (bit-shift-left 1 col)
                                            (bit-shift-left 1 (+ row col dimension))
                                            (bit-shift-left 1 (- row col dimension)))]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))


;; slightly faster with questionable type hinting
(defn queens0 [^long dimension]
  {:pre [(<= 0 dimension 13)]}
  (let [add-queens (fn [qvs ^long row]
                     (for [qv qvs :let [^long conflicts (peek qv)]
                           col (range dimension)
                           :let [^long col col
                                 rc (bit-or (bit-shift-left 1 col)
                                            (bit-shift-left 1 (+ row col dimension))
                                            (bit-shift-left 1 (- row col dimension)))]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))]
    
    (map pop (reduce add-queens [[0]] (range dimension)))))




;; a bit faster with everything inlined
(defn queens-inline [^long dimension]
  {:pre [(<= 0 dimension 13)]}
  (map pop (reduce (fn [qvs ^long row]
                     (for [qv qvs :let [^long conflicts (peek qv)]
                           col (range dimension)
                           :let [^long col col
                                 rc (bit-or (bit-shift-left 1 col)
                                            (bit-shift-left 1 (+ row col dimension))
                                            (bit-shift-left 1 (- row col dimension)))]
                           :when (zero? (bit-and conflicts rc))]
                       (conj (pop qv) col (bit-or conflicts rc))))
                   [[0]]
                   (range dimension))))

;; transduce version not faster, probably due to set-up overhead


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

;; for CLJS, we might need:
;; UNTESTED

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




;;; Richards approach but smashed into a single long for bookkeeping.  Explicit loop/recur
;;; for a tiny bit more performance.  Not pretty, but the fastest version I could come up with.

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

    (mapv pop (nth (iterate add-queens [[0]]) dimension))))


(defn nth-iter [f init n]
  (assert (not (neg? (long n))))
  (loop [r init cnt (long n)]
    (if (zero? cnt) r (recur (f r) (dec cnt)))))

(defn iqueens [^long dimension]
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

    (mapv pop (nth-iter add-queens [[0]] dimension))))



(defn fbits [f n]
  "Eagerly map `f` over the indices of the 1 bits in long `n`.  Returns a sequence with the
result corresponding to the most significant bit index first."
  (reduce (fn [res ^long b]
            (if (zero? b)
              (reduced res)
              (conj res (f (Long/numberOfTrailingZeros b)))))
          ()
          ;; Kernigan technique to clear lsb
          (iterate (fn [^long a] (bit-and a (dec a))) n)))

;; see also https://oeis.org/A129760


;; nice that the transducer pays off
;; pretty good, within 25% but not fastest

(defn xqueens [^long dimension]
  {:pre [(<= 0 dimension 14)]}
  (let [mask (dec (bit-shift-left 1 dimension))
        mask16 (bit-shift-left mask 16)
        mask32 (bit-shift-left mask 32)
        mask48 (bit-shift-left mask 48)
        
        shift-conflicts (fn [^long conflicts ^long q]
                          (let [conflicts (bit-or conflicts (bit-shift-left 0x0001000100010001 q))
                                rdiag (bit-and mask16 (unsigned-bit-shift-right conflicts 1))
                                ldiag (bit-and mask32 (bit-shift-left conflicts 1))
                                qcols (bit-and mask48 conflicts)]
                            (bit-or rdiag ldiag qcols
                                    (unsigned-bit-shift-right rdiag 16)
                                    (unsigned-bit-shift-right ldiag 32)
                                    (unsigned-bit-shift-right qcols 48))))
        
        add-queens (fn [qvs]
                     (into
                      []
                      (mapcat (fn [qv]
                                (let [^long conflicts (peek qv)
                                      pqv (pop qv)]
                                  (fbits (fn [^long q] (conj pqv q (shift-conflicts conflicts q)))
                                         (bit-and-not mask conflicts)))))
                      qvs))  ]
    
    (mapv pop (nth (iterate add-queens [[0]]) dimension))))

;; mapv is only a tiny bit faster, maybe not worth it


(set! *unchecked-math* false)







(defn fname [f]
  (let [stfn (str f)
        at (str/index-of stfn "@")
        dollar (str/index-of stfn "$")]
    (if at
      (str/replace (subs stfn (inc dollar) at) \_ \-)
      stfn)))

(defn bench [& args]
  (let [dims (or (seq (take-while pos-int? args)) '(8))
        fs (drop-while pos-int? args)
        bensum (fn [res] (transduce (map #(reduce + %)) + 0 res))]
    (when (seq fs)
      (doseq [dim dims]
        (let [answer (set ((first fs) dim))
              sum (bensum answer)]
          (doseq [f fs]
            (println)
            (println (str "(" (fname f) " " dim ")"))
            (let [res (f dim)]
              (assert (and (= (bensum res) sum)
                           (= (set (f dim)) answer))
                      (str (fname f) " failed")))
            (cc/quick-bench (bensum (f dim)))))))))


  
    
        
