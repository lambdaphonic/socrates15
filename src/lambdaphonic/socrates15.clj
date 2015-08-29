(ns lambdaphonic.socrates15
  (use [overtone.live :refer :all]
       [mud.core]
       [lambdaphonic.overtone.helpers]
       [lambdaphonic.overtone.synths]
       [overtone.inst.drum])
  (require [shadertone.tone :as t]))

(def scl '(0 2 3 4 7 9 12 14 15 14 12))

(comment
(def mtone (tone :amp 3 :freq (midi->hz (note :E1))))
(ctl mtone :gate 0)
)

(do
  (def bmoria [:E2 :E2 :D2 :D2 :C2 :C2 :B1 :B1 :E2 :E2 :A1 :B1 :E2 :E2 :B1 :B1 :C2 :C2 :A1 :B1])
  (def bass-line-moria (atom bmoria))
)

(defn bass [metro t beat]
  (at t
    (let [next-beat (+ beat 1)
          next-t (+ t (mspb metro))]
      (on-beat beat 2 2 #(do
                           (ctl mtone :freq (midi->hz (note (first @bass-line-moria))))
                          (reset! bass-line-moria (rotate (choose '(1 2 3 4)) @bass-line-moria)) ))

      (beat-map metro t beat next-beat 0.25
        (fn [b]
          (ctl mtone :amp (cosr b 1.5 2 (choose '(3/7 3/5))))))
      (apply-by next-t #'bass [metro next-t next-beat]))))

(comment
(bass metro (atbeat metro 1) 0)
)

(defn drums [metro t beat]
  (at t
    (let [next-beat (+ beat 1)
          next-t (+ t (mspb metro))]
      (dry-kick)
      (beat-map metro t beat next-beat 1/4
        (fn [b]
          (closed-hat2 :amp (cosr b 0.3 0.3 (choose '(3/5 1/3 3/7))))))
      (on-beat beat 2 2 #(do
                           (when (choose '(true false))
                            (open-hat))))

      (apply-by next-t #'drums [metro next-t next-beat]))))

(comment
(def metro (metronome 120))
(drums metro (atbeat metro 1) 0)
)

(defn melody [metro t beat]
  (at t
    (let [beats 1/4
          next-beat (+ beat beats)
          next-t (+ t (* beats  (mspb metro)))
          root (note (first @bass-line-moria))
          scl (scale root :minor)
          n (qcosr (scale-field :E :minor) beat (choose '(12 6 8)) (note :E4) (+ 8 (choose '(3/7 1/4 3/5))))
          ]
      (short-tone (midi->hz n)  :dur 1/4 :amp 1)
      (apply-by next-t #'melody [metro next-t next-beat]))))

(comment
(melody metro (atbeat metro 1) 0)
)


