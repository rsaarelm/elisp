;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C / C++

(setq c-basic-offset 2)

; Modified k&r. Don't indent namespaces since nested namespaces will then lead
; to unnecessary multiple indent levels.
(defconst personal-c-style
  '("k&r"
;    (c-offsets-alist . ((innamespace . 0) (defun-open . 0) ))))
    (c-offsets-alist . ((defun-open . 0) (inline-open . 0) ))))

(c-add-style "personal-c-style" personal-c-style)

(setq c-default-style '((other . "personal-c-style")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SML

;;; MLB modle
(autoload 'esml-mlb-mode "esml-mlb-mode")
(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(setq py-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua

(setq lua-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Factor

(let ((fuel-file "~/local/factor/misc/fuel/fu.el"))
  (when (file-exists-p fuel-file)
    (load-file fuel-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D

(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
