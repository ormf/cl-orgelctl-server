(defun edit-orgelctl-preset (str ref)
  (set-buffer (find-file-noselect orgel-preset-file))
  (delete-region (point-min) (point-max))
  (insert "(in-package :cl-orgelctl)\n\n;;; preset: ")
  (insert (format "%s\n\n" ref))
  (insert
   (replace-regexp-in-string ") :" ")\n :"
                             (replace-regexp-in-string "\n +(" " (" 
                                                       (replace-regexp-in-string "cl-orgelctl::" ""
                                                                                 (replace-regexp-in-string "orm-utils:" "" str)))))
;;;  (insert (format "\n\n(state-store-curr-preset %s)" ref))
  (insert "\n\n;;; (save-route-presets)")
  (delete-region (point) (point-max))
  (goto-char 34)
  (forward-line)
  (forward-line)
  (indent-sexp)
  (forward-line)
  (forward-line)
  (forward-line)
  (forward-line)
  (save-buffer)
  )

(defun edit-orgelctl-preset (str ref)
  (set-buffer (find-file-noselect orgel-preset-file))
  (delete-region (point-min) (point-max))
  (insert "(in-package :cl-orgelctl)\n\n;;; preset: ")
  (insert (format "%s\n\n" ref))
  (insert (replace-regexp-in-string "cl-orgelctl::" ""
                                    (replace-regexp-in-string "orm-utils:" "" str)))
;;;  (insert (format "\n\n(state-store-curr-preset %s)" ref))
  (insert "\n\n;;; (save-route-presets)")
  (delete-region (point) (point-max))
  (goto-char 34)
  (forward-line)
  (forward-line)
  (slime-reindent-defun)
  (forward-line)
  (forward-line)
  (forward-line)
  (forward-line)
  (save-buffer))

(defun next-orgelctl-preset ()
    (interactive)
    (slime-repl-send-string "(cl-orgelctl::next-route-preset)"))

(defun previous-orgelctl-preset ()
    (interactive)
    (slime-repl-send-string "(cl-orgelctl::previous-route-preset)")
    (save-excursion
      (switch-to-buffer (get-buffer "curr-preset.lisp"))))

(define-key lisp-mode-map (kbd "M-<left>") 'previous-orgelctl-preset)
(define-key lisp-mode-map (kbd "M-<right>") 'next-orgelctl-preset)

(save-excursion
  (switch-to-buffer (get-buffer "curr-preset.lisp")))


;;; 

 → 

;;;  +\(:[^:)]+ (\) → 
\1

;;; ) : → )
:
;;; 
 +( →  (


(replace-)

(setq test "(in-package :cl-orgelctl)

;;; preset: 10

(digest-route-preset
 10
 `(:preset nil
   :routes (:all (:main (main 1)) :orgel01
                 (:main (ccin 7) :bias-pos (ccin 21) :bias-bw (ccin 22) :osc-level
                        (bias-cos :bias-pos :bias-bw))
            :orgel02 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel03
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel04
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel05
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel06
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel07
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel08
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel09
                 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel10
                 (:osc-level (bias-cos :bias-pos :bias-bw)))))

;;; (save-route-presets)
")


