(in-package :cl-orgelctl)

(setf *route-presets*
#((:preset 0 :routes nil)
  (:preset nil :routes (:orgel01 (:level (bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
          (:orgel01
           (:level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw)))))
  (:preset nil :routes
          (:orgel01
           (:level (apply-notch :bias-type (bias-wippe :bias-pos :bias-bw)))))
  (:preset nil :routes
          (:orgel01
           (:level
            (apply-notch :bias-type
                         (bias-cos :bias-pos :bias-bw :levels
                                   #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
          (:orgel01
           (:level
            (apply-notch :bias-type
                         (bias-cos :bias-pos :bias-bw :levels
                                   #(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))))))
  (:preset nil :routes
          (:orgel01
           (:level
            (apply-notch :bias-type
                         (bias-wippe :bias-pos :bias-bw :levels
                                     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
          (:orgel01
           (:level
            (apply-notch :bias-type
                         (bias-wippe :bias-pos :bias-bw :levels
                                     #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                       1 0.5 1))))))
  (:preset nil :routes
          (:orgel01
           (:level
            (apply-notch :bias-type
                         (bias-wippe :bias-pos :bias-bw :levels
                                     #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                       1 0.5 1)))
            delay
            (apply-notch :bias-type
                         (bias-cos :bias-pos :bias-bw :levels
                                   #(1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                     1 0.5))))))
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil))
