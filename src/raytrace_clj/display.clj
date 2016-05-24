(ns raytrace-clj.display
  (:require [mikera.image.core :refer [width height show]])
  (:import [org.imgscalr Scalr]))

(defn show-progress
  [image pct filename tstart window?]
  (if window?
    (let [nx (width image)
          ny (height image)
          zoom (min (Math/floorDiv 1100 nx) (Math/floorDiv 700 ny) 4)]
      (if (> zoom 1)
        (show (Scalr/resize image
                            org.imgscalr.Scalr$Method/SPEED
                            org.imgscalr.Scalr$Mode/FIT_EXACT
                            (* zoom nx)
                            (* zoom ny)
                            nil)
              :title filename)
        (show image :title filename))))
  (let [elapsed-time (/ (- (System/currentTimeMillis) tstart) 1000.0)]
    (println (format "%.2fs, %d%%, ETA %.2fs"
                     elapsed-time
                     (int (* 100.0 pct))
                     (/ (* elapsed-time (- 1 pct)) pct)))))
