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


