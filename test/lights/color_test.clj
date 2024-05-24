(ns lights.color-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [lights.color :refer :all]))

; TODO: https://colorizer.org/ has translation tables! Also I asked a color
; expert about this and they said they'd never seen Hue's selection of matrix
; parameters before so ??? maybe that's why these numbers are so weird?

; See also the converter here: https://viereck.ch/hue-xy-rgb/

(def cases
  ; See https://colorizer.org/ and https://viereck.ch/hue-xy-rgb/
  [{:name :red
    :rgb (rgb 1 0 0)
    :hsl (hsl 0 1 0.5)
    :hsv (hsv 0 1 1)
    :xyz (xyz 0.4124 0.2126 0.0193)
    :xyy (xyy 0.6401 0.33 0.2126)
    :hue (hue 0.640 0.330 100)}
   {:name :green
    :rgb (rgb 0 1 0)
    :hsl (hsl 1/3 1 0.5)
    :hsv (hsv 1/3 1 1)
    :xyz (xyz 0.3576 0.7152 0.1192)
    :xyy (xyy 0.3 0.6 0.7152)
    :hue (hue 0.3 0.6 100)}
   {:name :blue
    :rgb (rgb 0 0 1)
    :hsl (hsl 2/3 1 0.5)
    :hsv (hsv 2/3 1 1)
    :xyz (xyz 0.1805 0.0722 0.9505)
    :xyy (xyy 0.15 0.06 0.0722)
    :hue (hue 0.15 0.06 99)}
   {:name :white
    :rgb (rgb 1 1 1)
    :hsl (hsl 0 0 1)
    :hsv (hsv 0 0 1)
    :xyz (xyz 0.9505 1 1.089) ; Z = 108.9, out of range?
    :xyy (xyy 0.3127 0.329 1)
    :hue (hue 0.313  0.329 100)}
   {:name :black
    :rgb (rgb 0 0 0)
    :hsl (hsl 0 0 0)
    :hsv (hsv 0 0 0)
    :xyz (xyz 0 0 0)
    :xyy (xyy 0 0 0)
    :hue (hue 0 0 0)}
   {:name :gray
    :rgb (rgb 0.5 0.5 0.5)
    :hsl (hsl 0 0 0.5)
    :hsv (hsv 0 0 0.5)
    :xyz (xyz 0.2017 0.2122 0.2311)
    :xyy (xyy 0.3127 0.3290 0.2122)
    :hue (hue 0.313 0.329 (/ 55 2.55))}
   {:name :mauve
    :rgb (rgb (/ 120 255) (/ 100 255.0) (/ 138 255.0))
    :hsl (hsl (/ 271.58 360) 0.1597 0.4667)
    :hsv (hsv (/ 271.58 360) 0.2754 0.5412)
    :xyz (xyz 0.1689 0.1494 0.2604)
    :xyy (xyy 0.2919 0.2582 0.1494)
    :hue (hue 0.292 0.260 (/ 65 2.55))}
   ])

(deftest color-test
  (doseq [c cases]
    ;(println)
    ;(println "Testing" (name (:name c)))
    (doseq [[space color] (dissoc c :name)]
      ;(println "Convert" color "to rgb")
      (is (=== (:rgb c) (->rgb color)))
      ;(println "Convert" color "to hsl")
      (is (=== (:hsl c) (->hsl color)))
      ;(println "Convert" color "to hsv")
      (is (=== (:hsv c) (->hsv color)))
      ;(println "Convert" color "to XYZ")
      (is (=== (:xyz c) (->xyz color)))
      ;(println "Convert" color "to xyY")
      (is (=== (:xyy c) (->xyy color)))
      ;(println "Convert" color "to hue")
      (is (=== (:hue c) (->hue color))))
    ))
