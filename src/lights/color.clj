(ns lights.color
  "Colorspace conversions"
  (:require [com.evocomputing.colors :as c]))

; See: http://www.brucelindbloom.com/index.html?Math.html
; See also: http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html

(def gamut-classes
  "Hue gamuts, from
  https://developers.meethue.com/documentation/supported-lights"
  {:B {:red   [0.675 0.322]
       :green [0.409 0.518]
       :blue  [0.167 0.04]}
   :C {:red   [0.6915 0.3038]
       :green [0.17 0.7]
       :blue  [0.1532 0.0475]}})

(def model->gamut-classes
  "Maps model strings to gamut classes"
  {"LCT001" :B
   "LCT007" :B
   "LCT010" :C
   "LCT014" :C
   "LCT002" :B
   "LCT003" :B
   "LCT011" :C})

(defn gamut
  "Maps model strings to gamuts"
  [model-str]
  (or (-> model-str model->gamut-classes gamut-classes)
      ; Default gamut
      (prn "Falling back to default gamut for" model-str)
      {:red   [1 0]
       :green [0 1]
       :blue  [0 0]}))

; Gamut pinning, adapted from https://developers.meethue.com/documentation/color-conversions-rgb-xy
(defn cross-product ; not a vector, what???
  [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* x2 y1)))

(defn closest-point-to-points
  "Takes a start point and an end point for a line, and a point p on the line.
  Returns the nearest point on the line."
  [[ax ay] [bx by] [px py]]
  (let [[apx apy] [(- px ax) (- py ay)]
        [abx aby] [(- bx ax) (- by ay)]
        ab2 (+ (* abx abx) (* aby aby))
        ap-ab (+ (* apx abx) (* apy aby))
        t (/ ap-ab ab2)
        t (max 0.0 (min 1.0 t))]
    [(+ ax (* abx t))
     (+ ay (* aby t))]))

(defn distance
  "Distance between two points."
  [[ax ay] [bx by]]
  (Math/sqrt (+ (Math/pow (- bx ax) 2)
                (Math/pow (- by ay) 2))))

(defn in-lamp-reach?
  "Takes an xy pair and a gamut. Returns true if the xy pair falls within the
  gamut."
  [[px py] {:keys [red green blue]}]
  (let [[rx ry] red
        [gx gy] green
        [bx by] blue
        v1 [(- gx rx) (- gy ry)]
        v2 [(- bx rx) (- by ry)]
        q  [(- px rx) (- py ry)]
        s (/ (cross-product q v2) (cross-product v1 v2))
        t (/ (cross-product v1 q) (cross-product v1 v2))]
    (and (<= 0 s)
         (<= 0 t)
         (<= (+ s t) 1))))

(defn clamp-angle
  "Clamps an angle to 0-1; negative values wrap around"
  [x]
  (mod x 1))

(defn clamp
  "Clamps a value to 0-1; negative values stick to the closest representable
  number"
  [x]
  (cond (< x 0) 0
        (< 1 x) 1
        true    x))

(defn srgb-gamma
  "Applies a gamma transformation to an srgb' value to obtain an srgb value.
  See
  https://www.image-engineering.de/library/technotes/958-how-to-convert-between-srgb-and-ciexyz."
  [v]
  (if (<= v 0.0031308)
    (* v 12.92)
    (- (* 1.055 (Math/pow v (/ 2.4))) 0.055)))

(defn srgb-ungamma
  "Applies a gamma transformation to an srgb value to obtain a linear srgb'
  value. See
  https://www.image-engineering.de/library/technotes/958-how-to-convert-between-srgb-and-ciexyz."
  [v]
  (if (<= v 0.04045)
    (/ v 12.92)
    (Math/pow (/ (+ v 0.055) 1.055) 2.4)))

(defprotocol Color
  (=== [a b] "Approximate equality"))
(defprotocol ToRGB (->rgb [c]))
(defprotocol ToHSL (->hsl [c]))
(defprotocol ToHSV (->hsv [c]))
(defprotocol ToXYZ (->xyz [c]))
(defprotocol ToXYY (->xyy [c]))
(defprotocol ToHue
  (->hue [c]))

;; Accessors

(defn h [c] (:h (->hsv c)))
(defn s [c] (:s (->hsv c)))
(defn v [c] (:v (->hsv c)))
(defn l [c] (:l (->hsl c)))

(defn r [c] (:r (->rgb c)))
(defn g [c] (:g (->rgb c)))
(defn b [c] (:b (->rgb c)))

(defn assoc-h [c h] (-> c ->hsl (assoc :h (clamp-angle h))))
(defn assoc-s [c s] (-> c ->hsl (assoc :s (clamp s))))
(defn assoc-l [c l] (-> c ->hsl (assoc :l (clamp l))))
(defn assoc-v [c v] (-> c ->hsv (assoc :v (clamp v))))

(defn update-h [c f & args] (assoc-h c (apply f (h c) args)))
(defn update-s [c f & args] (assoc-s c (apply f (s c) args)))
(defn update-l [c f & args] (assoc-l c (apply f (l c) args)))
(defn update-v [c f & args] (assoc-v c (apply f (v c) args)))

(defn perturb-h
  "Perturbs hue by a random amount up to dh in hue space"
  [c dh]
  (update-h c (rand-nth [+ -]) (rand dh)))

(defn perturb-s
  "Perturbs saturation by a random amount up to ds in sat space"
  [c ds]
  (update-s c (rand-nth [+ -]) (rand ds)))

(defn perturb-l
  "Perturbs lightness by a random amount up to dl in lightness space"
  [c dl]
  (update-l c (rand-nth [+ -]) (rand dl)))

(defn perturb-v
  "Perturbs HSV value by a random amount up to dv"
  [c dv]
  (update-v c (rand-nth [+ -]) (rand dv)))

; Representations

(declare hex)
(declare hsl)
(declare hsv)
(declare rgb)
(declare xyz)
(declare xyy)
(declare hue)

(defn max-Y
  "Adapted from https://viereck.ch/hue-xy-rgb/ColorSpace.js. No idea what this
  does."
  ([x y]
   (max-Y x y 10))
  ([x y iterations]
   (loop [i   iterations
          bri 1]
     (if (= i 0)
       bri
       (let [c (->rgb (xyy x y bri))
             m (max (max (:r c) (:g c)) (:b c))]
         (if (zero? m)
           ; Totally guessing here that Y is bounded to 1. Is it??? I've spent
           ; so long in xyY docs and I can't actually tell.
           (recur (dec i) 1)
           (recur (dec i) (/ bri m))))))))

(defrecord Hex [hex]
  ToRGB (->rgb [_]
          (rgb (double (/ (Long/parseLong (subs hex 0 2) 16) 255))
               (double (/ (Long/parseLong (subs hex 2 4) 16) 255))
               (double (/ (Long/parseLong (subs hex 4 6) 16) 255))))
  ToHSV (->hsv [c] (-> c ->rgb ->hsv))
  ToHSL (->hsl [c] (-> c ->rgb ->hsl))
  ToXYZ (->xyz [c] (-> c ->rgb ->xyz))
  ToXYY (->xyy [c] (-> c ->xyz ->xyy))
  ToHue (->hue [c] (-> c ->rgb ->hue)))

(defn n===
  "Numeric approximate comparison"
  ([a b]
   (let [r (if (or (zero? a) (zero? b))
             (and (< -0.01 a 0.01)
                  (< -0.01 b 0.01))
             ; I think the colorspace.js conversion used by
             ; https://viereck.ch/hue-xy-rgb/ might be wrong--we get results
             ; that are 2% off sometimes. Close enough though.
             (< 0.97 (/ a b) 1.03))]
     #_(if-not r
       (prn a "=/=" b))
     r))
  ([a b c]
   (and (=== a b)
        (=== a c))))

(extend-protocol Color
  Float              (=== [a b] (n=== a b))
  Double             (=== [a b] (n=== a b))
  Long               (=== [a b] (n=== a b))
  clojure.lang.Ratio (=== [a b] (n=== a b)))

; sRGB coordinates, all from 0 to 1.
(defrecord RGB [r g b]
  Color (=== [a b]
          (and (=== (:r a) (:r b))
               (=== (:g a) (:g b))
               (=== (:b a) (:b b))))

  ToRGB (->rgb [c] c)

  ToHSL (->hsl [c] (-> c ->hsv ->hsl))

  ToHSV (->hsv [_]
          (let [min-rgb (min r g b)
                max-rgb (max r g b)
                delta   (- max-rgb min-rgb)]
            (if (zero? delta)
              ; Grey
              (hsv 0 0 max-rgb)
              (hsv (condp = max-rgb
                     ; Reds
                     r (/ (mod (/ (- g b) delta) 6) 6)

                     ; Greens
                     g (/ (+ (/ (- b r) delta) 2) 6)

                     ; Blues
                     (/ (+ (/ (- r g) delta) 4) 6))
                   (/ delta max-rgb)
                   max-rgb))))

  ToXYZ (->xyz [c]
          ; Gamma correction
          (let [r (srgb-ungamma r)
                g (srgb-ungamma g)
                b (srgb-ungamma b)]
            ; Wide RGB D65 conversion, as per
            ; https://developers.meethue.com/documentation/color-conversions-rgb-xy
            ;(xyz (+ (* r 0.664511) (* g 0.154324) (* b 0.162028))
            ;     (+ (* r 0.283881) (* g 0.668433) (* b 0.047685))
            ;     (+ (* r 0.000088) (* g 0.072310) (* b 0.986039)))))
            ; This matrix seems broken, use
            ; https://www.image-engineering.de/library/technotes/958-how-to-convert-between-srgb-and-ciexyz
            ; instead.
            (xyz (+ (* r 0.4124564) (* g 0.3575761) (* b 0.1804375))
                 (+ (* r 0.2126729) (* g 0.7151522) (* b 0.0721750))
                 (+ (* r 0.0193339) (* g 0.1191920) (* b 0.9503041)))))

  ToXYY (->xyy [c] (-> c ->xyz ->xyy))

  ToHue
  (->hue [c]
    (-> c ->xyz ->hue)))

; HSL (all from 0 to 1)
(defrecord HSL [h s l]
  Color (=== [a b]
          (or ; All HSL colors with L=0 or 1 are equal
              (n=== 0 (:l a) (:l b))
              (n=== 1 (:l a) (:l b))
              ; All HSL colors with 0 saturation and equal lightness are
              ; equivalent
              (and (n=== 0 (:s a) (:s b))
                   (n=== (:l a) (:l b)))
              ; Otherwise, compare normally
              (and (n=== (:h a) (:h b))
                   (n=== (:s a) (:s b))
                   (n=== (:l a) (:l b)))))

  ToRGB (->rgb [_]
          ; Adapted from https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB_alternative
          (let [a (* s (min l (- 1 l)))
                f (fn f [n]
                    (let [k (mod (+ n (* h 12)) 12)]
                      (- l (* a
                              (max -1 (min (- k 3)
                                           (- 9 k) 1))))))]
            (rgb (f 0) (f 8) (f 4))))

  ToHSL (->hsl [c] c)
  ToHSV (->hsv [c]
          (let [v (+ l (* s (min l (- 1 l))))]
            (hsv h
                 (if (== v 0)
                   0
                   (* 2 (- 1 (/ l v))))
                 v)))
  ToXYZ (->xyz [c] (-> c ->rgb ->xyz))
  ToXYY (->xyy [c] (-> c ->xyz ->xyy))

  ToHue
  (->hue [c] (-> c ->xyz ->hue)))

(defrecord HSV [h s v]
  Color (=== [a b]
          ; Colors with 0 value are all black
          (or (n=== 0 (:v a) (:v b))
              ; Colors with 0 saturation are equal if their values are
              (and (n=== 0 (:s a) (:s b))
                   (n=== (:v a) (:v b)))
              ; Otherwise compare normally
              (and (=== (:h a) (:h b))
                   (=== (:s a) (:s b))
                   (=== (:v a) (:v b)))))

  ToHSV (->hsv [c] c)
  ToHSL (->hsl [c]
          (let [l (* v (- 1 (/ s 2)))]
            (hsl h
                 (if (or (== l 0) (== l 1))
                   0
                   (/ (- v l)
                      (min l (- 1 l))))
                 l)))
  ToHue (->hue [c] (-> c ->hsl ->hue))
  ToRGB (->rgb [c] (-> c ->hsl ->rgb))
  ToXYZ (->xyz [c] (-> c ->hsl ->xyz))
  ToXYY (->xyy [c] (-> c ->hsl ->xyy))
  )

(defrecord XYZ [X Y Z]
  Color (=== [a b]
          (and (=== (:X a) (:X b))
               (=== (:Y a) (:Y b))
               (=== (:Z a) (:Z b))))

  ToXYZ (->xyz [c] c)

  ToXYY
  (->xyy [c]
    ; From http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_xyY.html
    (if (zero? (+ X Y Z))
      (xyy 0 0 0)
      (xyy (/ X (+ X Y Z))
           (/ Y (+ X Y Z))
           Y)))

  ToRGB
  (->rgb [c]
    (let [; Hue thinks we should use a "Wide RGB D65 conversion" at //developers.meethue.com/develop/application-design-guidance/color-conversion-formulas-rgb-to-xy-and-back/#xy-to-rgb-color, but this doesn't seem to return sensible values.
          ; r (+ (* X 1.656492)  (* Y -0.354851) (* Z -0.255038))
          ; g (+ (* X -0.707196) (* Y 1.655397)  (* Z 0.036152))
          ; b (+ (* X 0.051713)  (* Y -0.121364) (* Z 1.011530))
          ; I'm going to use the matrix from https://www.image-engineering.de/library/technotes/958-how-to-convert-between-srgb-and-ciexyz instead.
          ; Reverse gamma correction: these values are in [0, 1]
          r (+ (* X  3.2404542) (* Y -1.5371385) (* Z -0.4985314))
          g (+ (* X -0.9692660) (* Y  1.8760108) (* Z  0.0415560))
          b (+ (* X  0.0556434) (* Y -0.2040259) (* Z  1.0572252))
          r (srgb-gamma r)
          g (srgb-gamma g)
          b (srgb-gamma b)]
      (rgb (max 0 r) (max 0 g) (max 0 b))))

  ToHue (->hue [c]
          (let [; Convert to CIE XY coordinates
                x (if (zero? X) X (/ X (+ X Y Z)))
                y (if (zero? Y) Y (/ Y (+ X Y Z)))
                max-Y (max-Y x y)
                bri (* 100 (clamp (/ Y max-Y)))]
            (hue x y bri)))

  ToHSV
  (->hsv [c]
    (-> c ->rgb ->hsv))

  ToHSL
  (->hsl [c]
    (-> c ->rgb ->hsl)))

; Represents x y Y coordinates
(defrecord XYY [x y Y]
  Color (=== [a b]
          (and (=== (:x a) (:x b))
               (=== (:y a) (:y b))
               (=== (:Y a) (:Y b))))

  ToXYY
  (->xyy [c] c)

  ToXYZ
  ; From http://www.brucelindbloom.com/index.html?Eqn_xyY_to_XYZ.html
  (->xyz [c]
    (if (zero? y)
      (xyz 0 0 0)
      (xyz (/ (* x Y) y)
           Y
           (/ (* (- 1 x y) Y) y))))

  ToRGB
  (->rgb [c] (-> c ->xyz ->rgb))

  ToHSV
  (->hsv [c] (-> c ->xyz ->hsv))

  ToHSL
  (->hsl [c] (-> c ->xyz ->hsl))

  ToHue
  (->hue [c] (-> c ->xyz ->hue)))


; xy are a pair from 0 to 1, bri is 0-100. This is the native representation in
; the Hue API.
(defrecord Hue [x y bri]
  Color (=== [a b]
          (and (=== (:x a)   (:x b))
               (=== (:y a)   (:y b))
               (=== (:bri a) (:bri b))))

  ToXYZ
  (->xyz [c] (-> c ->xyy ->xyz))

  ToXYY
  ; See https://developers.meethue.com/develop/application-design-guidance/color-conversion-formulas-rgb-to-xy-and-back/
  ; ... which appears to be very wrong. Let's use http://www.brucelindbloom.com/index.html?Eqn_xyY_to_XYZ.html and https://viereck.ch/hue-xy-rgb/.
  (->xyy [c]
    (let [max-Y (max-Y x y)]
      (xyy x y (* max-Y (/ bri 100)))))

  ToRGB
  (->rgb [c]
    (-> c ->xyz ->rgb))

  ToHSV
  (->hsv [c]
    (-> c ->xyz ->hsv))

  ToHSL
  (->hsl [c]
    (-> c ->xyz ->hsl))

  ToHue
  (->hue [c] c))

; Constructors
(defn hex [hex]   (Hex. hex))
(defn hsl [h s l] (HSL. (mod h 1) s l))
(defn hsv [h s v] (HSV. (mod h 1) s v))
(defn rgb [r g b] (RGB. r g b))

(defn rgb256
  "RGB, but in the range [0, 256)."
  [r g b]
  (rgb (/ r 255) (/ g 255) (/ b 255)))

(defn xyz [x y z] (XYZ. x y z))
(defn xyy [x y Y] (XYY. x y Y))

(defn hue
  ([xy bri]
   (cond (vector? xy)
         (let [[x y] xy]
           (hue x y bri))

         (map? xy)
         (hue (:x xy) (:y xy) bri)

         true
         (assert false (str (pr-str xy) " should be a map or vector"))))
  ([x y bri]
   (assert x (pr-str x))
   (assert (<= 0 x 1) (str (pr-str x) " should be between 0 and 1"))
   (assert y (pr-str y))
   (assert (<= 0 y 1) (str (pr-str y) " should be between 0 and 1"))
   (assert bri (pr-str bri))
   (assert (<= 0 bri 100) (str (pr-str bri) " should be between 0 and 100"))
   (Hue. x y bri)))
