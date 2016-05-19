(ns raytrace-clj.display
  (:require [mikera.image.core :refer [width height show]])
  (:import [org.imgscalr Scalr]))

(defn show-progress
  [image j filename tstart]
  (let [elapsed-time   (/ (- (System/currentTimeMillis) tstart) 1000.0)
        nx (width image)
        ny (height image)
        zoom (min (Math/floorDiv 1280 nx) (Math/floorDiv 800 ny) 4)]
    (if (> zoom 1) 
      (show (Scalr/resize image
                          org.imgscalr.Scalr$Method/SPEED
                          org.imgscalr.Scalr$Mode/FIT_EXACT
                          (* zoom nx)
                          (* zoom ny)
                          nil)
            :title filename)
      (show image :title filename))
    (println (format "%.2fs, row %d / %d, %d%%, ETA %.2fs"
                     elapsed-time (inc j) ny
                     (int (* 100.0 (/ (inc j) ny)))
                     (/ (* elapsed-time (- ny (inc j))) (inc j))))))
