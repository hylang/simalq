#!/usr/bin/env hy

"Draw an SVG map of a level with colored lines showing which
teleportation gate goes where."


(require
  hyrule [defmain])
(import
  hy.pyops *
  random
  colorsys
  drawsvg :as dw
  simalq.un-iq [iq-quest])
(setv  T True  F False)


(defn drawing [#* elems #** kwargs]
  (setv x (dw.Drawing #** kwargs))
  (.extend x elems)
  x)

(defn group [#* elems #** kwargs]
  (setv x (dw.Group #** kwargs))
  (.extend x elems)
  x)


(defn make-svg [output-path mapo]
  (setv tiles (lfor
    column mapo.data
    stack column
    tile stack
    tile))

  (random.seed 100)
  (setv  size-factor 20  x-factor 1.2)

  (defn xy [pos [jitter F]]
    (lfor
      coord [(* pos.x x-factor) (- mapo.height pos.y 1)]
      (+ coord (if jitter (/ (random.randint -9 9) 100) 0))))

  (setv gates (lfor
    tile tiles
    :if (= tile.stem "gate")
     #(tile (.format "#{:02x}{:02x}{:02x}" #* (gfor
       c (colorsys.hsv-to-rgb (random.random) 1 1)
       (round (* c 255)))))))

  (setv d (drawing
    :width (* mapo.width size-factor x-factor) :height (* mapo.height size-factor)
    (group
      :transform f"scale({size-factor}) translate(.5 .5)"
      ; Draw base tiles.
      (group
        :font-family "DejaVu Sans Mono,monospace"
        #* (gfor
          tile tiles
          (dw.Text
            tile.mapsym
            #** (dict (zip "xy" (map + (xy tile.pos) [-.5 .25])))
            :font-size "1px"
            :align "left")))
      ; Draw gate destinations
      (group :fill-opacity .4 #* (gfor
        [tile color] gates
        (dw.Circle
          #* (xy tile.target)
          :r .5
          :fill color)))
      ; Draw gate connection lines
      (group :stroke-width .2 :stroke-opacity .4 #* (gfor
        [tile color] gates
        (dw.Line
          #* (xy tile.pos :jitter T)
          #* (xy tile.target :jitter T)
          :stroke color))))))

  (.save-svg d output-path))


(defmain [_ output-path quest-name level-n]
  (make-svg
    output-path
    (. (iq-quest quest-name) levels [(- (int level-n) 1)] map)))
