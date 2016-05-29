(ns raytrace-clj.core
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]
            [raytrace-clj.hitable :refer :all]
            [raytrace-clj.shader :refer :all]
            [raytrace-clj.camera :refer :all]
            [raytrace-clj.scene :refer :all]
            [raytrace-clj.display :refer [show-progress]]
            [mikera.image.core :refer [new-image set-pixel save]]
            [mikera.image.colours :refer [rgb-from-components]])
  (:gen-class))

(mat/set-current-implementation :vectorz)

(defn color
  "compute color for ray up to an optional recursion depth"
  ([r world] (color r world 50))
  ([r world depth]
   (if-let [hrec (hit? world r 0.001 Float/MAX_VALUE)]
     ;; hit, return scattered ray unless recursion depth too deep
     (if-let [scat (and (pos? depth)
                        (scatter (:material hrec) r hrec))]
       (mat/add (emitted (:material hrec) (:uv hrec) (:p hrec))
                (mat/mul (:attenuation scat)
                         (color (:scattered scat) world (dec depth))))
       ;; if depth too deep return only emitted (if any)
       (emitted (:material hrec) (:uv hrec) (:p hrec)))
     ;; else miss, return black
     (vec3 0 0 0))))

(defn pixel
  "compute color for a pixel up to an optional recursion depth"
  ([i j nx ny nr camera world] (pixel i j nx ny nr camera world 50))
  ([i j nx ny nr camera world depth]
   (->>
    (repeatedly nr #(vector (/ (+ (float i) (rand)) nx)
                            (/ (+ (float j) (rand)) ny)))
    (map (fn [[u v]] (color (get-ray camera u v) world depth)))
    (reduce mat/add)
    (mat/mul (/ 1.0 nr))
    (mat/sqrt)
    (mat/mul 255.99)
    (mat/emap int)
    (seq))))

(defn tiled-coords
  "lazy sequence describing image as tiles, where each tile
  consists of a hash-map {:pct pct, :chunk ([i1 j1] [i2 j2] ...)}"
  [width height chunk-size]
  (let [coords (for [j (range height) i (range width)] [i j])
        chunks (Math/ceil (/ (double (* width height)) chunk-size))
        pcts   (map #(/ (inc %) chunks) (range (inc chunks)))]
    ;; above we computed the (i,j) pairs as a lazy sequence
    ;; below we are partitioning into chunks
    ;; and merging the percent completion for progress reporting
    (map #(hash-map :chunk %1 :pct %2)
         (partition chunk-size chunk-size [] coords)
         pcts)))

(defn -main
  [& [name ix iy is win]]
  (let [tstart (System/currentTimeMillis)
        filename (or name "render.png")
        nx (if ix (Integer/parseUnsignedInt ix) 200)
        ny (if iy (Integer/parseUnsignedInt iy) 100)
        nr (if is (Integer/parseUnsignedInt is) 100)
        window? (or (= win "true") (= win "win"))
        image (new-image nx ny)
        lookfrom (vec3 13 2 3)
        lookat (vec3 0 0 0)
        camera (make-thin-lens-camera lookfrom
                                      lookat
                                      (vec3 0 1 0)
                                      45
                                      (/ (float nx) (float ny))
                                      0.0
                                      10.0
                                      0.0
                                      1.0)
        ;world (make-bvh (make-two-spheres) 0.0 1.0)
        ;world (make-bvh (make-two-perlin-spheres) 0.0 1.0)
        ;world (make-bvh (make-textured-sphere) 0.0 1.0)
        world (make-bvh (make-example-light) 0.0 1.0)
        ;; world (make-bvh (make-random-scene 11 true) 0.0 1.0)
        ]

    ;; pre-open display window before spawning threads
    (show-progress image 0 filename tstart window?)

    ;; render in parallel w/ chunks determined by tiled-coords
    (dorun
     (pmap
      (fn [{:keys [pct chunk]}]
        (doseq [[i j] chunk]
          (let [[ir ig ib] (pixel i j nx ny nr camera world)]
            (set-pixel image i (- (dec ny) j)
                       (rgb-from-components ir ig ib))))
        (show-progress image pct filename tstart window?))
      (tiled-coords nx ny 32)))

    (save image filename)
    (println "wrote" filename)))
