(ns lights.color
  "Colorspace conversions"
  (:require [com.evocomputing.colors :as c]))


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

(defprotocol ToRGB (->rgb [c]))
(defprotocol ToHSL (->hsl [c]))
(defprotocol ToHSV (->hsv [c]))
(defprotocol ToXYZ (->xyz [c]))
(defprotocol ToHue
  (->hue [c model])
  (->hue-hsb [c model]))

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

(defn update-h [c f & args] (assoc-h c (apply f (h c) args)))
(defn update-s [c f & args] (assoc-s c (apply f (s c) args)))
(defn update-l [c f & args] (assoc-l c (apply f (l c) args)))

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

; Representations

(declare hex)
(declare xyz)
(declare hsl)
(declare hsv)
(declare rgb)
(declare hue)

(defrecord Hex [hex]
  ToRGB (->rgb [_]
          (rgb (double (/ (Long/parseLong (subs hex 0 2) 16) 255))
               (double (/ (Long/parseLong (subs hex 2 4) 16) 255))
               (double (/ (Long/parseLong (subs hex 4 6) 16) 255))))
  ToHSL (->hsl [c] (-> c ->rgb ->hsl))
  ToXYZ (->xyz [c] (-> c ->rgb ->xyz))
  ToHue (->hue-hsb [c model] (-> c ->rgb (->hue-hsb model))))

; RGB coordinates, all from 0 to 1.
(defrecord RGB [r g b]
  ToRGB (->rgb [c] c)

  ToHSL (->hsl [_]
          (let [[h s l] (:hsl (c/create-color {:r (int (* 255 r))
                                               :g (int (* 255 g))
                                               :b (int (* 255 b))}))]
            (hsl (/ h 255) (/ s 255) (/ l 255))))

  ToHSV (->hsv [_]
          (let [min-rgb (min r g b)
                max-rgb (max r g b)
                delta   (- max-rgb min-rgb)]
            (if (= min-rgb max-rgb)
              (hsv 0 0 max-rgb) ; grey
              (let [dr (/ (+ (/ (- max-rgb r) 6) (/ delta 2)) delta)
                    dg (/ (+ (/ (- max-rgb g) 6) (/ delta 2)) delta)
                    db (/ (+ (/ (- max-rgb b) 6) (/ delta 2)) delta)
                    h (condp = max-rgb
                        r (- db dg)
                        g (+ 1/3 (- dr db))
                        b (+ 2/3 (- dg dr)))]
                (hsv h (/ delta max-rgb) max-rgb)))))

  ToXYZ (->xyz [c]
          ; Gamma correction
          (let [r (if (< 0.04045 r)
                    (Math/pow (/ (+ r 0.055) 1.055) 2.4)
                    (/ r 12.92))
                g (if (< 0.04045 g)
                    (Math/pow (/ (+ g 0.055) 1.055) 2.4)
                    (/ g 12.92))
                b (if (< 0.04045 b)
                    (Math/pow (/ (+ b 0.055) 1.055) 2.4)
                    (/ b 12.92))]
            ; Wide RGB D65 conversion, as per
            ; https://developers.meethue.com/documentation/color-conversions-rgb-xy
            (xyz (+ (* r 0.664511) (* g 0.154324) (* b 0.162028))
                 (+ (* r 0.283881) (* g 0.668433) (* b 0.047685))
                 (+ (* r 0.000088) (* g 0.072310) (* b 0.986039)))))

  ToHue
  (->hue [c model]
    (-> c ->xyz (->hue model)))

  (->hue-hsb [c model]
    (-> c ->hsv (->hue-hsb model))))

; HSL (all from 0 to 1)
(defrecord HSL [h s l]
  ToRGB (->rgb [_]
          (let [[r g b a] (:rgba (c/create-color {:h (int (* 255 h))
                                                  :s (int (* 100 s))
                                                  :l (int (* 100 l))}))]
            (rgb (/ r 255) (/ g 255) (/ b 255))))
  ToHSL (->hsl [c] c)
  ToHSV (->hsv [c]
          (let [v (+ l (* s (min l (- 1 l))))]
            (hsv h
                 (if (== v 0)
                   0
                   (* 2 (- 1 (/ l v))))
                 v)))
  ToXYZ (->xyz [c] (-> c ->rgb ->xyz))

  ToHue
  (->hue [c model]     (-> c ->xyz (->hue model)))
  (->hue-hsb [c model] (-> c ->hsv (->hue-hsb model))))

(defrecord HSV [h s v]
  ToHSV (->hsv [c] c)
  ToHSL (->hsl [c]
          (let [l (* v (- 1 (/ s 2)))]
            (hsl h
                 (if (or (== l 0) (== l 1))
                   0
                   (/ (- v l)
                      (min l (- 1 l))))
                 l)))
  ToHue (->hue-hsb [c model]
             ; Philips hue uses a nonlinear hue mapping WHY IS THIS SO HARD
             ; https://developers.meethue.com/documentation/lights-api#16_set_light_state
             ; (prn :h h :s s :v v)
             {:hue (let [m 65535
                         r 0
                         g 25500
                         b 46920] ; maaaagic
                     (long (condp < h
                             2/3 (+ b (* 3 (- h 2/3) (- m b)))
                             1/3 (+ g (* 3 (- h 1/3) (- b g)))
                             (* 3 h g))))
              :sat (long (* 255 s))
              :bri (long (* 255 v))}))

(defrecord XYZ [X Y Z]
  ToXYZ (->xyz [c] c)

  ToRGB
  (->rgb [c]
    ; See https://developers.meethue.com/develop/application-design-guidance/color-conversion-formulas-rgb-to-xy-and-back/#xy-to-rgb-color
    (let [; Wide RGB D65 conversion
          r (+ (* X 1.656492)  (* Y -0.354851) (* Z -0.255038))
          g (+ (* X -0.707196) (* Y 1.655397)  (* Z 0.036152))
          b (+ (* X 0.051713)  (* Y -0.121364) (* Z 1.011530))
          ; Reverse gamma correction: these values are in [0, 1]
          r (if (<= r 0.0031308)
              (* 12.92 r)
              (- (* 1.055 (Math/pow r (/ 2.4))) 0.055))
          g (if (<= g 0.0031308)
              (* 12.92 g)
              (- (* 1.055 (Math/pow g (/ 2.4))) 0.055))
          b (if (<= g 0.0031308)
              (* 12.92 b)
              (- (* 1.055 (Math/pow b (/ 2.4))) 0.055))]
      (rgb (max 0 r) (max 0 g) (max 0 b))))

  ToHue (->hue [c model]
          (let [;gamut (gamut model)
                ; Convert to CIE XY coordinates
                x (if (zero? X) X (/ X (+ X Y Z)))
                y (if (zero? Y) Y (/ Y (+ X Y Z)))
                p [x y]
                bri (long (* Y 255))]
            ; I think the lamp actually does gamut pinning for us?
            (hue p bri)
            ;(if (in-lamp-reach? p gamut)
            ;  (hue p bri)
            ;  ; Nope, outside gamut
            ;  (let [pab (closest-point-to-points (:red gamut) (:green gamut) p)
            ;        pac (closest-point-to-points (:blue gamut) (:red gamut) p)
            ;        pbc (closest-point-to-points (:green gamut) (:blue gamut) p)
;
;                    ; Take closest point in gamut
;                    closest (first (sort-by (partial distance p)
;                                            [pab pac pbc]))]
;                (prn p :out-of-gamut :pinned-to closest)
;                (hue p bri)))))
))

  ToHSV
  (->hsv [c]
    (-> c ->rgb ->hsv))

  ToHSL
  (->hsl [c]
    (-> c ->rgb ->hsl)))

; xy are a pair from 0 to 1, bri is 0-255. This is the native representation in
; the Hue API.
(defrecord Hue [xy bri]
  ToXYZ
  ; See https://developers.meethue.com/develop/application-design-guidance/color-conversion-formulas-rgb-to-xy-and-back/
  (->xyz [c]
    ; TODO: gamut check?
    (let [[x y] xy
          z (- 1.0 x y)
          Y (/ bri 255)
          X (* (/ Y y) x)
          Z (* (/ Y y) z)]
      (xyz X Y Z)))

  ToRGB
  (->rgb [c]
    (-> c ->xyz ->rgb))

  ToHSV
  (->hsv [c]
    (-> c ->xyz ->hsv))

  ToHSL
  (->hsl [c]
    (-> c ->xyz ->hsl)))

; Constructors
(defn hex [hex]   (Hex. hex))
(defn hsl [h s l] (HSL. (mod h 1) s l))
(defn hsv [h s v] (HSV. (mod h 1) s v))
(defn rgb [r g b] (RGB. r g b))
(defn xyz [x y z] (XYZ. x y z))
(defn hue [xy bri] (Hue. xy bri))
