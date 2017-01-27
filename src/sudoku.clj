(ns sudoku
  (:require [clojure.set :as set]))

>(def board identity)

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
    (for [x (range tlr (+ tlc bsv))
          y (range tlc (+ tlr bsh))]
      [x y])))

;; (defn block-pairs [coords]
;;   (let [[x y] (top-left coords)]
;;     (for [[x' y'] (coord-pairs [0 1 2])]
;;       [(+ x' x) (+ y' y)])))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords board  coord))))


;; (defn block-values [board coord]
;;   nil)

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)


;-----------------------------------------







(defn block-values [board coords]
  (set(map (fn [args] (value-at board args))
           (block-pairs coords))))












