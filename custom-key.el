; Bind autocomplete to C-Tab
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

; Use C-q as backward-kill-word. The quoted-insert is rare and can be beyond a
; more complex binding.

(global-set-key "\C-q" 'backward-kill-word)
(global-set-key "\C-\M-q" 'quoted-insert)

; Accessing various modes from anywhere

; Allow F9 to serve as a prefix key
(global-set-key (kbd "<f9>") (make-sparse-keymap))

; Show calendar
(global-set-key (kbd "<f9> c") 'calendar)
; Show calculator
(global-set-key (kbd "<f9> l") 'calc)
; Go to currently clocked task
(global-set-key (kbd "<f9> o") 'org-clock-goto)
; Change clocked task to one in history.
(global-set-key (kbd "<f9> t") (lambda () (interactive) (org-clock-in '(4))))

; Show org agenda
(global-set-key (kbd "<f10>") 'org-agenda)

; Regular compile, prompt for compile command.
(global-set-key (kbd "S-<f5>") 'compile)
; Quick compile, run the same compile command as last time.
(global-set-key (kbd "<f5>") (lambda () (interactive) (compile compile-command)))

; Don't use suspend on Windows, it's not the concern of Emacs there and
; doesn't even work right on tiling WMs with no concept of hiding windows.
; Instead, follow the original idea of accessing a shell and run ansi-term.
(if window-system
    (global-set-key "\C-z" 'ansi-term))

; Buffer and window navigation

(defun exhume-buffer ()
  "Switch to the buffer at the bottom of the buffer list. Opposite to (bury-buffer)."
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

(defun in-other-buffer (f)
  "Perform a function when switched to the other window."
  (interactive)
  (other-window 1)
  (funcall f)
  (other-window -1))

; Buffer navigation
(global-set-key [f12] 'exhume-buffer)
(global-set-key [f11] 'bury-buffer)

(global-set-key [C-f12] (lambda () (interactive) (in-other-buffer 'exhume-buffer)))
(global-set-key [C-f11] (lambda () (interactive) (in-other-buffer 'bury-buffer)))

; Replace the useless default buffer list with a better one.
(global-set-key "\C-x\C-b" 'buffer-menu)

; Window navigation
(global-set-key (kbd "C-,") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-.") 'other-window)

; Vim-style jumping to the opposing paren
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)
