(in-package :cl-orgelctl)

(recall-preset 0 1 0.5)
(recall-preset 0)
(recall-preset 1)
(recall-preset 2)

(defun automate-interp (num1 num2 duration)
  (let* ((dtime 0.04)
         (stepinc (/ dtime duration))
         (i 0)
         (end (+ (cm:now) duration)))
    (labels ((inner (time i)
               (if (<= time end)
                   (progn
                     (recall-preset num1 num2 i)
                     (let ((next (+ time dtime)))
                       (cm:at next #'inner next (incf i stepinc))))
                   (recall-preset num2))))
      (inner (cm:now) i))))

(automate-interp 0 1 40)
(automate-interp 1 0 40)

(untrace)

(digest-routes
 `(:orgel01
   (:level01 (+ (mlevel 1 1) (delay 1 1) -14)
    :level02 (+ (level 1 1) (* -1 (q 1 1)) 5)
    :level03 (+ (level 1 2) (* -1 (q 1 2)) -13)
    :level04 (+ (level 1 3) 5))))

(digest-route-preset
 1
 `(:preset 0
   :routes (:orgel01
            (:level01 (+ (mlevel 1 1) (delay 1 1) -14)
             :level02 (+ (level 1 1) (* -1 (q 1 1)) 5)
             :level03 (+ (level 1 2) (* -1 (q 1 2)) -13)
             :level04 (+ (level 1 3) 5)))))

(save-route-presets)


(digest-routes nil)
