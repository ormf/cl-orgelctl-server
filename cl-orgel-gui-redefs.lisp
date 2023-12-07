;;; 
;;; cl-orgel-gui-redefs.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-orgel-gui)

#|
(defun make-orgel-attr-val-receiver (slot orgelidx global-orgel-ref)
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
             (orgel-val (read-from-string val-string)))
        (set-cell (slot-value global-orgel-ref slot-symbol) orgel-val :src self)))))
|#

;;; (cl-orgelctl::orgel-ctl :orgel01 :bias-type 0)

;;; (cl-orgelctl::orgel-ctl :orgel01 :phase -1.0)

(defun make-orgel-val-receiver (slot orgelidx global-orgel-ref)
  (declare (ignore orgelidx))
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
             (orgel-val (read-from-string val-string)))
;;;        (format t "make-orgel-val-receiver: ~a~%" orgel-val)
        (set-cell (slot-value global-orgel-ref slot-symbol) orgel-val :src self)))))

(defun make-orgel-array-receiver (slot orgelidx global-orgel-ref)
  (declare (ignorable orgelidx))
  (let ((g-accessor (slot->function "g-orgel" slot))
        (accessor (slot->function "orgel" slot)))
    (declare (ignore g-accessor))
    (lambda (idx val self)
      (let* ((orgel-val (read-from-string (ensure-string val))))
 ;;;        (format t "array-received: orgel~2,'0d ~a ~a~%" (1+ orgelidx) idx orgel-val)
        (set-cell (aref (funcall accessor global-orgel-ref) idx) orgel-val :src self)))))

(defun make-orgel-kbd-array-receiver (slot global-orgel-ref)
  (let ((g-accessor (slot->function "g-orgel" slot))
        (accessor (slot->function "orgel" slot)))
    (declare (ignore g-accessor))
    (lambda (idx val self)
      (let* ((orgel-val (read-from-string (ensure-string val)))
             (orgel-ref (aref cl-orgelctl::*orgel-freqs-vector* idx))
             (orgel-idx (1- (third orgel-ref)))
             (array-idx (1- (fourth orgel-ref))))
        (format t "kbd-array-received: orgel~2,'0d ~a ~a ~a~%" (1+ orgel-idx) array-idx idx orgel-val)
        (set-cell (aref (funcall accessor (aref global-orgel-ref orgel-idx)) array-idx)
                  orgel-val :src self)))))

(defun create-preset-panel (container vu-container)
  (let ((preset-panel
          (create-div container :height 80
                                :css '(:border "thin solid black" :position absolute :top 0 :left 0 :display none :justify-content space-between :width 100% :height 80px))))
    (create-div preset-panel :content "Presets" :style "margin: 2px;")
    (let* ((prv (init-button preset-panel :content "prev" :active-bg "orange"
                                          :background "#bbb" :style "font-size: 8px;"))
           (nb (numbox preset-panel :size 6 :min 0 :max 127))
           (nxt (init-button preset-panel :content "next" :active-bg "orange"
                                          :background "#bbb" :style "font-size: 8px;")))
      (set-on-click
       prv
       (lambda (obj)
         (declare (ignore obj))
         (let ((curr (value nb)))
           (when (> curr (min-val nb))
             (setf (value nb) (1- curr))))))
      (set-on-click
       nxt
       (lambda (obj)
         (declare (ignore obj))
         (let ((curr (value nb)))
           (when (< curr (max-val nb))
             (setf (value nb) (1+ curr))))))
      (create-br preset-panel)
      (let ((recall-btn
              (init-button preset-panel :content "recall" :active-bg "orange"
                                        :background "#d5ffd5" :style "font-size: 8px;"))
            (store-btn
              (init-button preset-panel :content "store" :active-bg "orange"
                                        :background "#ffd5d5" :style "font-size: 8px;"))
            load-btn
            save-btn)
        (create-br preset-panel)
        (setf load-btn (init-button preset-panel :content "load" :active-bg "orange"
                                                 :background "#d5ffd5" :style "font-size: 8px;"))
        (setf save-btn (init-button preset-panel :content "save" :active-bg "orange"
                                                 :background "#ffd5d5" :style "font-size: 8px;"))
        (set-on-click
         recall-btn
         (lambda (obj)
           (declare (ignore obj))
           (cl-orgelctl::recall-orgel-preset (round (value nb)))))
        (set-on-click
         store-btn
         (lambda (obj)
           (declare (ignore obj))
           (cl-orgelctl::store-orgel-preset (round (value nb)))))
        (set-on-click
         load-btn
         (lambda (obj)
           (declare (ignore obj))
           (cl-orgelctl::load-orgel-presets)))
        (set-on-click
         save-btn
         (lambda (obj)
           (declare (ignore obj))
           (cl-orgelctl::save-orgel-presets)))
        (install-preset-key-switch container (html-id vu-container) (html-id preset-panel))))))


(defmacro init-kbd-multi-vu (slot parent gui-orgeln global-orgel
                         &key (num 8) (width "80px") (height "100px") (background "#444") (direction :up) (border "none")
                           (inner-background "var(--vu-background)") (inner-border "thin solid black") (inner-padding "0")
                           (inner-padding-bottom "0px")
                           (led-colors :blue) (css '(:margin-bottom 10px :position absolute :top 0 :left 0))
                           val-change-cb)
  (declare (ignore orgelidx global-orgel))
  (let ((vus (gensym "vus"))
        (mvu (gensym "mvu"))
        (g-meter-array (gensym "g-meter-array"))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
;;;        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        )
    `(let* ((,mvu (multi-vu ,parent :num ,num :width ,width :height ,height
                                    :led-colors ,led-colors
                                    :direction ,direction :background ,background
                                    :inner-background ,inner-background
                                    :border ,border :inner-border ,inner-border
                                    :inner-padding-bottom ,inner-padding-bottom
                                    :inner-padding ,inner-padding
                                    :css ,css
                                    :val-change-cb ,val-change-cb))
            (orgel-freq-vector cl-orgelctl::*orgel-freqs-vector*)
            (,vus (meters ,mvu))
            (,g-meter-array (coerce
                             (loop for i below *orgelcount*
                                   collect (make-array 16))
                             'vector)))
       
       (loop for idx below *orgelcount* do (setf (,g-accessor (aref gui-orgeln idx)) (aref ,g-meter-array idx)))
       (loop for vu across ,vus
             for idx from 0
             do (let* ((orgel-ref (aref orgel-freq-vector idx))
                          (orgel-idx (1- (third orgel-ref)))
                          (array-idx (1- (fourth orgel-ref))))
                  (setf (attribute vu "data-db") (- (val (aref (aref *orgel-mlevel* orgel-idx) array-idx)) 100))
                  (setf (aref (aref ,g-meter-array orgel-idx) array-idx) vu)))
       ,mvu)))

(defun create-orgel-kbd-gui (container gui-orgeln global-orgel-ref)
      (let* (p1 p2 p3 p4 p5 p7 nbs1 nbs2 tg1-container tg2-container)
        (create-br container)
        (setf p1  (create-div container :css '(:margin-left "10px" :width "1800px")))
;;;        (create-div p1 :content (format nil "Orgel~2,'0d" (1+ orgelidx)) :css '(:align "bottom" :padding-bottom 10px))
        (setf p4  (create-div p1 :width 180 :height 150 :css '(:display flex :justify-content space-between :flex "0 0 auto")))
        (setf p3  (create-div p4))
        (setf p2 (create-div p3 :css '(:width 160px :height 60px :display flex :justify-content space-between :margin-bottom 10px)))
        (setf nbs1 (create-div p2 :css '(:width 75px :font-size 6pt :display flex :flex-direction column :justify-content space-between))) ;;; container for numberbox(es)
        (setf nbs2 (create-div p2 :css '(:width 73px  :font-size 6pt :display flex :flex-direction column :justify-content space-between))) ;;; container for numberbox(es)
 
       (let ((vu-container
                (init-kbd-multi-vu :meters p1 gui-orgeln global-orgel-ref
                                   :num 160 :width "1800px" :height "80px"
                                   :led-colors :blue
                                   :direction :up :background "#444"
                                   :inner-background "#444"
                                   :border "none" :inner-border "thin solid black"
                                   :inner-padding-bottom "0px"
                                   :inner-padding "0"
                                   :css '(:margin-bottom 100px :margin-top 20px :margin-left 10px :position absolute :top 0 :left 0)
                                   :val-change-cb nil))))
;;           (when (zerop orgelidx)
;;             (create-preset-panel p7 vu-container)))
;; ;;;        main volume slider
;;         (init-vslider :main p4 orgelidx orgel global-orgel-ref :height "150px" :css '(:margin-left 5px))
;;         (create-div p1 :height 10) ;;; distance
         (create-div p1 :content "Level" :css *msl-title-css*)
;;         (setf p5 (create-div p1 :width 180 :height 100 :style "padding-bottom: 5px;display: flex;justify-content: space-between;flex: 0 0 auto;"))
;; ;;;        (setf p6 (create-div p5 :style "display: block;"))
        (let ((msl
                (apply #'multi-slider p1 :num 160 :css '(:margin-top 40px :width 1800)
                        :val-change-cb (make-orgel-kbd-array-receiver :osc-level global-orgel-ref)
                       *msl-style*)))
;;;          (init-vslider :bias-bw p5 orgelidx orgel global-orgel-ref :width "8px" :height "100px")
          (loop for vsl across (sliders msl)
                for idx from 0
                do (let* ((orgel-ref (aref cl-orgelctl::*orgel-freqs-vector* idx))
                          (orgel-idx (1- (third orgel-ref)))
                          (array-idx (1- (fourth orgel-ref))))
                     (setf (value vsl) (val (aref (orgel-level (aref global-orgel-ref orgel-idx)) array-idx)))
                     (setf (aref (g-orgel-osc-level (aref gui-orgeln orgel-idx)) array-idx) vsl))))
;;         (init-hslider :bias-pos p1 orgelidx orgel global-orgel-ref :height "8px" :width "160px")
;; 
;;         (dolist (label '("Delay" "Q" "Gain" "Osc-Level"))
;;           (let ((slot-name (make-symbol (format nil "~:@(~a~)" label))))
;;             (let ((msl
;;                     (create-slider-panel
;;                      p1
;;                      :label label
;;                      :val-change-cb (make-orgel-array-receiver slot-name orgelidx global-orgel-ref))))
;; ;;;        (create-br p1)
;;               (let ((g-accessor-fn (slot->function "g-orgel" slot-name))
;;                     (accessor-fn (slot->function "orgel" slot-name)))
;;                 (loop for vsl across (sliders msl)
;;                       for idx from 0
;;                       do (progn
;;                            (setf (value vsl) (val (aref (funcall accessor-fn global-orgel-ref) idx)))
;;                            (setf (aref (funcall g-accessor-fn orgel) idx) vsl)))))))
        ))

(defun orgel-kbd (body)
  (let ((orgel-gui (make-orgel-gui))
        connection-id)
    (clog-dsp-widgets-initialize body)
    (setf connection-id (clog::connection-id body))
    (setf (title (html-document body)) "Orgel Keyboard Sliders")
    (add-class body "w3-blue-grey") ;;; background color
    (setf (gethash "orgel-gui" (gethash connection-id clog-connection::*connection-data*))
          orgel-gui)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (with-connection-cache (body)
      (let ((gui-container (create-div body
                                       :css '(:display "flex"
                                              :overflow "auto"
                                              :margin-right "15px"
                                              :padding-bottom "30px"))))
        (create-orgel-kbd-gui gui-container (orgel-gui-orgeln orgel-gui) *curr-state*)))))

(set-on-new-window 'orgel-kbd :path "/kbd")
