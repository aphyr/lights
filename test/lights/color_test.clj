(ns lights.color-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [lights.color :refer :all]))

(deftest hue->hsl-test
  ; This is a reasonably intense red at max brightness, from the Hue API
  (let [c (hue [0.6801 0.3028] 255)]
    ; Not even fucking close? I think this is because the Hue gamut is bigger
    ; than RGB? At least it IS mostly red
    (is (= (rgb 1 0 0) (->rgb c)))))
