(ns tetris.core
  (:use tetris.game)
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent MouseEvent MouseListener))
  (:gen-class))

;;;;;;Controls;;;;
(defn handle-input [#^KeyEvent event]
  (condp = (.getKeyCode event)
    KeyEvent/VK_W(swap! OFFSET #(update % :y inc))
    KeyEvent/VK_E(swap! OFFSET #(-> %
                                    (update :y inc)
                                    (update :x dec)))
    KeyEvent/VK_D(swap! OFFSET #(update % :x dec))
    KeyEvent/VK_X(swap! OFFSET #(update % :y dec))
    KeyEvent/VK_Z(swap! OFFSET #(-> %
                                    (update :y dec)
                                    (update :x inc)))
    KeyEvent/VK_A(swap! OFFSET #(update % :x inc))

    KeyEvent/VK_LEFT (swap! OFFSET #(update % :x inc))
    KeyEvent/VK_RIGHT (swap! OFFSET #(update % :x dec))
    KeyEvent/VK_UP (swap! OFFSET #(update % :y inc))
    KeyEvent/VK_DOWN (swap! OFFSET #(update % :y dec))
    ))

(defn input-listener []
  (proxy [ActionListener KeyListener] []
    (actionPerformed [e])
    (keyPressed [e] (handle-input e))
    (keyReleased [e])
    (keyTyped [e])))

(defn handle-mouse-click [#^MouseEvent event]
  (condp = (.getButton event)
    MouseEvent/BUTTON1 (reset! CLICK [(.getX event) (.getY event)])))

(defn mouse-listener []
  (proxy [MouseListener] []
    (mouseClicked [e] (handle-mouse-click e))
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])))

;;;;;;;UI;;;;;;;;;
(def colors {"black"  Color/black
             "blue"   Color/blue
             "green"  Color/green
             "yellow" Color/yellow
             "orange" Color/orange
             "pink"   Color/pink
             "red"    Color/red})

(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g (.getDrawGraphics buffer)]
    (try
      (draw-fn g)
      
      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn gex-x [] (map #(* % 0.6) (list 0 0.866 0.866 0 -0.866 -0.866)))
(defn gex-y [] (map #(* % 0.66) (list -1 -0.5 0.5 1 0.5 -0.5)))

(defn x-to-pos [pointx x y]
  (let [width (/ @WIDTH COLS)]
    (-> pointx
         (+ (+ x (* 0.5 y)))
         (* width)
         (+ (/ @HEIGHT 2)))))

(defn y-to-pos [pointy x y]
  (let [width (/ @WIDTH COLS)]
    (-> pointy
         (+ y)
         (* width)
         (+ (/ @WIDTH 2)))))


(defn draw-gex [x y color #^Graphics g]
  (let [xpos (->> (gex-x)
                  (map #(x-to-pos % x y))
                  (int-array))
        ypos (->> (gex-y)
                  (map #(y-to-pos % x y))
                  (int-array))]
    (doto g
      (.setColor (get colors color))
      (.fillPolygon xpos ypos 6)
      (.setColor Color/gray)
      (.drawPolygon xpos ypos 6))
    [(/ (apply + xpos) 6) (/ (apply + ypos) 6)]))

(defn draw-text [#^Graphics g color #^String text x y]
  (doto g
    (.setColor color)
    (.drawString text  (int x) (int y))))

(defn inView? [x y]
  (and (> x (- (@view :x-position) (@view :half-width)))
       (< x (+ (@view :x-position) (@view :half-width)))
       (> y (- (@view :y-position) (@view :half-height)))
       (< y (+ (@view :y-position) (@view :half-height)))))

(defn draw-view [board]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 @WIDTH @HEIGHT))

    (doseq [square (range (count board))]
      (let [[x y] (pos-to-xy square)]
        (if (inView? x y)
          (let
            [x-view (- x (@view :x-position))
             y-view (- y (@view :y-position))
             [xpos ypos] (draw-gex x-view y-view (get board square) g)]
            (draw-text g Color/green (format "%s:%s" x y) (- xpos 7) ypos)))))

    (draw-text g Color/green (format "x: %s y: %s" (@view :x-position) (@view :y-position)) 20 25)))

(defn -main [& args]
  (reset! WIDTH 600)
  (reset! HEIGHT 600)
  (let [frame (JFrame. "Tetris")
        canvas (Canvas.)]
    (doto frame
      (.setSize @WIDTH (+ (/ @HEIGHT ROWS) @HEIGHT))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))

    (doto canvas
      (.createBufferStrategy 2)
      (.addKeyListener (input-listener))
      (.addMouseListener (mouse-listener))
      (.setVisible true)
      (.requestFocus))

    ;;game loop
    (loop [board (get-board)
           block (get-block)
           old-time (System/currentTimeMillis)]

      (reset! OFFSET {:x 0 :y 0})
      (reset! ROTATION nil)
      (reset! CLICK nil)
      (Thread/sleep 10)
      (draw canvas (draw-view board))

      (let [cur-time (System/currentTimeMillis)
            new-time (long (if (> (- cur-time old-time) 250)
                             cur-time
                             old-time))
            drop? (> new-time old-time)
            new-board (update-board-on-click board)]
        ;[num-removed new-board] (clear-lines board)]

        (recur
          new-board
          (transform board block false)
          new-time)))))

;(-main)
