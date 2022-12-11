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

(defun invert-if (invert? fn)
  (lambda (x)
    (+ (if invert? 127 1)
       (* (if invert? -1 1)
          (funcall fn x)))))





(funcall (bias-wippe 3 1) 1)


(digest-routes nil)

(digest-routes
 `(:orgel01
   (:level (invert-if (= (bias-type 1) 1)
                      (cos-bias (bias-pos 1) (bias-bw 1))))))

(digest-routes
 `(:orgel01
   (:level (apply-notch (bias-type 1) (cos-bias (bias-pos 1) (bias-bw 1))))))

(recalc-bias-pos (bias-pos 1))
(recalc-bw (bias-bw 1))
Zitronenreibe Microplane

(digest-routes
 `(:orgel01
   (:level (apply-notch (bias-type 1) (bias-wippe (bias-pos 1) (bias-bw 1))))))


(digest-routes
 `(:orgel01
   (:level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw)))))

(digest-routes
 `(:orgel01
   (:delay (apply-notch :bias-type (bias-wippe :bias-pos :bias-bw
                                             :levels #(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)))
    :level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw
                                             :levels #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))

(clear-routes)



(digest-routes
 `(:orgel01
   (:level (apply-notch :bias-type (bias-wippe :bias-pos :bias-bw)))))

(digest-routes
 `(:orgel01
   (:level (apply-notch :bias-type (bias-wippe :delay01 :q01)))))

(digest-routes
 `(:orgel01
   (:level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw
                                             :levels #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))

(digest-routes
 `(:orgel01
   (:level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw
                                             :levels #(1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5))))))

(digest-routes
 `(:orgel01
   (:level01 (+ (mlevel 1 1) (delay 1 1) -0.14)
    :level02 (+ (level 1 1) (* -1 (q 1 1)) 0.15)
    :level03 (+ (level 1 2) (* -1 (q 1 2)) -0.13)
    :level04 (+ (level 1 3) 0.15))))

(register-responders :level01 :orgel01 '(+ (mlevel 1 1) (delay 1 1) -14) t)

(get-fn-form :level01 :orgel01 (replace-keywords '(+ (mlevel 1 1) (delay 1 1) -14) (get-orgel-no :orgel01)))

(defun get-fn-form (target orgel form)
  (let* ((call-spec (gethash target *orgel-preset-def-lookup*))
         (orgeltarget orgel))
    `(lambda () (,(first call-spec) ,orgeltarget
            ,@(rest call-spec) ,form))))

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
