(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 9})


(def sudoku-board-1
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board-1
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))


(def invalid-board-1
  (board [[5 3 4 6 7 8 9 1 1]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
[3 4 5 2 8 6 1 7 9]]))


(def all-values
  #{1 2 3 4 5 6 7 8 9})

;[[5 3 0 | 0 7 0 | 0 0 0]
; [6 0 0 | 1 9 5 | 0 0 0]
; [0 9 8 | 0 0 0 | 0 6 0]
; -------+-------+-------
; [8 0 0 | 0 6 0 | 0 0 3]
; [4 0 0 | 8 0 3 | 0 0 1]
; [7 0 0 | 0 2 0 | 0 0 6]
; -------+-------+-------
; [0 6 0 | 0 0 0 | 2 8 0]
; [0 0 0 | 4 1 9 | 0 0 5]
; [0 0 0 | 0 8 0 | 0 7 9]]


;; (defn get-top-left [row col]
;;   (let [(topl-coords (set (range 0 (count sudoku-board-1) 3)))]
;;     (loop [topleft [row col]]
;;       (cond (top1-coords )))))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (board row)))

(defn col-values [board [_ col]]
  (set (map  #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

;coords -> top-left -> block coords -> values

;; need to strengthen it to work with any board size
;; condier using mod size and substract the diff instead of recuring by dec

(defn top-left  [board [c r]]
  (let [tl-coords (set (range 0 (count board) 3))]
    (loop [c c
           r r]
      (cond
        (not (or (contains? tl-coords c)
                 (contains? tl-coords r)))
        (recur (dec r) (dec c))

        (and (contains? tl-coords c)
             (contains? tl-coords r))
        [c r]

        (contains? tl-coords c)
        (recur (dec r) c)

        :else (recur r (dec c))))))

;; (defn top-left [coords]
;;     (map (fn [i] (* (int (/ i 3)) 3)) coords))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coords  [board coord]
  (let [[tlr tlc] (top-left board  coord)
        count (count board)
        bsh (int (Math/ceil (Math/sqrt count)))
        bsv (int (Math/floor (Math/sqrt count)))]
    (for [x (range tlr (+ tlr bsv))
          y (range tlc (+ tlc bsh))]
      [x y])))

;; (defn block-pairs [coords]
;;   (let [[x y] (top-left coords)]
;;     (for [[x' y'] (coord-pairs [0 1 2])]
;;       [(+ x' x) (+ y' y)])))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords board coord))))


;; (defn block-values [board coord]
;;   nil)

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference (set (range 1 10))
                      (set/union (block-values board coord)
                                 (col-values board coord)
                                 (row-values board coord)))))

(defn filled? [board]
  (every? #(has-value? board %) (coord-pairs (range 0 9))))

(defn rows [board]
  (map set board))

(defn set-diff [set]
  (seq (set/difference (set (range 1 10)) set)))

(defn valid-rows? [board]
  (every? (partial = all-values) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (every? (partial = all-values) (rows board)))

(defn blocks [board]
  (let [blocks-tls-coords (for [x [0 3 6]
                                y [0 3 6]]
                            [x y])]
    (map #(block-values board %) blocks-tls-coords)))

;; (defn blocks [board]
;;   (for [x (range 0 9 3) y (range 0 9 3)]
;;     (block-values board [x y])
;; ))

(defn valid-blocks? [board]
 (every? (partial = all-values) (blocks board)) )

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some #(and (zero? (value-at  sudoku-board-1 %)) %) (coord-pairs (range 0 9))))

(defn solve [board]
  nil)


;-----------------------------------------

;; (defn block-values [board coords]
;;   (set(map (fn [args] (value-at board args))
;;            (block-pairs coords))))
