(ns tetris.game)

(def WIDTH (atom nil))
(def HEIGHT (atom nil))

(def COLS 20)
(def ROWS 20)

(def view (atom {:x-position (int (/ COLS 2))
                 :y-position (int (/ COLS 2))
                 :half-width 5
                 :half-height 5
                 :angle 0}))

(def OFFSET (atom {:x 0 :y 0}))
(def ROTATION (atom nil))
(def CLICK (atom nil))
(def COLORS ["black" "red" "blue" "green" "yellow" "orange" "pink"])
(def COLORS-N  (zipmap COLORS (rest COLORS)))
(defn color-next [color] (get COLORS-N color (first COLORS)))
(defn color--next [color]
  (let [colors* (cycle COLORS)]
    (->> color
         (.indexOf colors*)
         (inc)
         (nth colors*))))
(def SHAPES [[[0 1] [0 2] [0 3] [0 4]]
             [[0 0] [0 1] [1 1] [1 2]]
             [[1 2] [1 1] [0 1] [0 0]]
             [[0 1] [1 1] [1 0] [2 1]]
             [[0 0] [0 1] [1 0] [1 1]]
             [[0 0] [0 1] [0 2] [1 2]]
             [[1 0] [1 1] [1 2] [0 2]]])

(defn get-block []
  (let [shape (rand-nth SHAPES)
        offset (inc (rand-int (- COLS 3)))]
    {:color (rand-nth (rest COLORS))
     :shape (map (fn [[x y]] [(+ x offset) y]) shape)}))

(defn get-board []
  (vec (take (* ROWS COLS) (repeatedly #(nth COLORS (rand-int (.length COLORS)))))))

(defn pos-to-xy [pos]
  (let [x (mod pos COLS)
        y (int (/ (- pos x) COLS))]
    [x, y]))

(defn xy->canvas [x y]
  [(* x (/ @WIDTH COLS)) (* y (/ @WIDTH COLS))])

(defn canvas->xy
  ([x y] [(int (/ x (/ @WIDTH COLS))) (int (/ y (/ @WIDTH COLS)))])
  ([xy] (canvas->xy (first xy) (second xy))))

(defn collides?
  ([board x y pos]
    (let [[posx posy] (pos-to-xy pos)]
      (and
        (> x (- 1))
        (< x COLS)
        (< y ROWS)
        (not (and
               (= posx x)
               (= posy y)
               (not= "black" (get board (+ pos COLS))))))))
  ([board shape pos]
    (every?
      #{true}
      (for [[x y] shape]
        (collides? board x y pos))))
  ([board shape]
    (not (reduce
           #(and %1 (collides? board shape %2))
           (range (count board))))))

(defn rotate [board shape]
  (if @ROTATION
    (let [[avg-x avg-y] (->> shape
                          (reduce
                            (fn [[tx ty] [x y]]
                              [(+ tx x) (+ ty y)]))
                          (map #(int (/ % 4))))

          rotated (map (fn [[x y]]
                         [(int (+ avg-x (- y avg-y)))
                          (int (- avg-y (- x avg-x)))])
                       shape)]
      (if (collides? board rotated)
        shape rotated))
    shape))

(defn shift [board shape]
  (swap! view (fn [view*]
                (-> view*
                    (update :x-position #(+ % (:x @OFFSET)))
                    (update :y-position #(+ % (:y @OFFSET))))))
  shape)

(defn transform [board {:keys [color shape]} drop?]
  (let [rotated (->> shape (shift board) (rotate board))]
    {:color color
     :shape (if drop?
              (map (fn [[x y]] [x (inc y)]) rotated)
              rotated)}))

(defn update-board-on-click [board]
  (if @CLICK
    (vec (map #(let [[x y] (pos-to-xy %)]
                 (if (and (= [x y] (canvas->xy @CLICK)))
                   (color--next (get board %))
                   (get board %)))
              (range (count board))))
    board))

