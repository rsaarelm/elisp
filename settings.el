; Kill the GUI clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; Do not use physical tabs.
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)

; Insert automatic line breaks in text mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 78)

; Do not use doubled space after a period.
(setq sentence-end-double-space nil)

; Syntax highlighting is nice.
(global-font-lock-mode 1)

; Highlighted selections are also nice.
(transient-mark-mode 1)

(mouse-wheel-mode t)

; Backup files are messy, maybe we don't want them.
(setq make-backup-files nil)

; No bell.
(setq visible-bell t)

; No blink
(blink-cursor-mode 0)

; Start the server so we can use emacsclient
; This doesn't work in MS Windows, so we use the conditional.
(when (and first-evaluation-of-dot-emacs (not (eq window-system 'w32)))
   (server-start))

; I like to see everything on the screen
(set-variable 'truncate-lines t)

; And with modern monitors I can even keep my partial width windows at least
; 80 columns wide.
(set-variable 'truncate-partial-width-windows nil)

; Move between windows with shift + arrow keys. (The default compatibility fix
; with org-mode only handles shift-arrows)
(windmove-default-keybindings)

; We've seen it already.
(setq inhibit-startup-message t)

; Quicker line removal. Use C-k to erase even non-empty lines when at column 1.
(setq kill-whole-line t)

; Autocleanse end-of-line whitespace.
; (Commented out for now, can't be used when developing code collaboratively
; since it messes up patches with whitespace elimination noise.)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Show the trailing whitespace anyway.
(setq-default show-trailing-whitespace t)

; Paste at cursor position when mouse-pasting
(setq mouse-yank-at-point t)

; Don't ask about following symlinks to version-controlled files.
(setq vc-follow-symlinks nil)

; Only highlight the latest perceived misspelling. Don't clutter buffers with
; highlight junk from false positives.
(setq flyspell-persistent-highlight nil)

