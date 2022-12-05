(in-package :cl-orgelctl)

(setf *route-presets*
#((preset 1 routes
          (orgel01
           (level01 (+ (mlevel 1 1) (delay 1 1) -14) level02
            (+ (level 1 1) (* -1 (q 1 1)) 5) level03
            (+ (level 1 2) (* -1 (q 1 2)) -13) level04 (+ (level 1 3) 5))))
  (preset 0 routes
          (orgel01
           (level01 (+ (mlevel 1 1) (delay 1 1) -14) level02
            (+ (level 1 1) (* -1 (q 1 1)) 5) level03
            (+ (level 1 2) (* -1 (q 1 2)) -13) level04 (+ (level 1 3) 5))))
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil))