;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preliminaries

; We might want to do something only when Emacs is first run and have other
; things be re-evaluated while the .emacs is being developed further. Make a
; variable for it.

(defvar first-evaluation-of-dot-emacs t)

; Help debugging errors in .emacs

(setq debug-on-error t)

(defun fullpath-relative-to-current (relative-path)
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  relative-path))

(defun relative-load (file) (load (fullpath-relative-to-current file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual settings

(condition-case nil
  (progn
    (relative-load "lib/color-theme-solarized.el")

    (if window-system
      (color-theme-solarized-dark)
      (color-theme-hober)))
  (error nil))

(relative-load "settings.el")
(relative-load "util.el")
(relative-load "orgmode.el")
(relative-load "prog.el")
(relative-load "custom-key.el")

(condition-case nil
    (progn
      (require 'yasnippet)
      (yas/load-directory "~/.elisp/snippets")
      (yas/initialize))
  (error (message "Yasnippet not found.")))

(load-elisp-directory "local")

(setq debug-on-error nil)
