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
    KeyEvent/VK_LEFT (swap! OFFSET #(map + [-1 0] %))
    KeyEvent/VK_RIGHT (swap! OFFSET #(map + [1 0] %))
    KeyEvent/VK_UP (reset! ROTATION :left)
    KeyEvent/VK_DOWN (reset! ROTATION :right)))

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

(defn gex-x [] (map #(* % 0.66) (list -1 -0.5 0.5 1 0.5 -0.5)))
(defn gex-y [] (map #(* % 0.6) (list 0 0.866 0.866 0 -0.866 -0.866)))

(defn draw-gex [x y color #^Graphics g]
  (let [width (/ @WIDTH COLS)
        height (/ @HEIGHT ROWS)
        xpos (->> (gex-x)
                  (map #(+ % x))
                  (map #(* % width))
                  (int-array))
        ypos (->> (gex-y)
                  (map #(+ % (+ y (* 0.5 (rem x 2)))))
                  (map #(* % width))
                  (int-array))]
    (doto g
      (.setColor (get colors color))
      (.fillPolygon xpos ypos 6)
      (.setColor Color/gray)
      (.drawPolygon xpos ypos 6))))

(defn draw-text [#^Graphics g color #^String text x y]
  (doto g
    (.setColor color)
    (.drawString text  (int x) (int y))))

(defn draw-game-over [score]
  (fn [#^Graphics g]
    (doto g
      (.setColor (new Color (float 0) (float 0) (float 0) (float 0.7)))
      (.fillRect 0 0 @WIDTH @HEIGHT))
    (draw-text g Color/red "GAME OVER" (- (/ @WIDTH 2) 50) (/ @HEIGHT 2))
    (draw-text g Color/red (str "Final Score: " score) (- (/ @WIDTH 2) 55) (+ 15 (/ @HEIGHT 2)))))

(defn draw-board [board block score]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 @WIDTH @HEIGHT))

    (doseq [square (range (count board))]
      (let [[x y] (pos-to-xy square)]
        (draw-gex x y (get board square) g)))

    (doseq [[x y] (:shape block)]
      (draw-gex x y (:color block) g))

    (draw-text g Color/green (str "score: " score) 20 25)))

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
    (loop [score 0
           board (get-board)
           block (get-block)
           old-time (System/currentTimeMillis)]

      (reset! OFFSET [0 0])
      (reset! ROTATION nil)
      (reset! CLICK nil)
      (Thread/sleep 10)
      (draw canvas (draw-board board block score))

      (let [cur-time (System/currentTimeMillis)
            new-time (long (if (> (- cur-time old-time) 250)
                             cur-time
                             old-time))
            drop? (> new-time old-time)
            [num-removed new-board] (clear-lines (update-board-on-click board block))]
            ;[num-removed new-board] (clear-lines board)]
        (cond
          (game-over? board)
          (draw canvas (draw-game-over score))

          (collides? board (:shape block))
          (recur
            score
            (update-board board block)
            (get-block)
            new-time)

          :default
          (recur
            (+ score (* num-removed num-removed))
            new-board
            (transform board block drop?)
            new-time))))))

;(-main)
