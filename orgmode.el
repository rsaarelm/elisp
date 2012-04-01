; Some settings inspired by http://doc.norang.ca/org-mode.html

; Prevent conflict with shift-dir keys for switching buffer
; The config variable used to be org-CUA-compatible.
(setq org-replace-disputed-keys t)

; Activate org-protocol to that we can access org from external programs.
;(require 'org-protocol)

; Start org-mode for .org files.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

; Custom settings in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            ; yasnippet
;            (condition-case nil (progn
;                                  (make-variable-buffer-local 'yas/trigger-key)
;                                  (setq yas/trigger-key [tab])
;                                  (define-key yas/keymap [tab] 'yas/next-field-group))
;              (error (message "Yasnippet not found.")))
            ; flyspell for automatic spell checking
            (flyspell-mode 1)))

; Remember old clocks
(setq org-clock-history-length 10)

; Make the clock persist across sessions
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

; Restart clock from old time if there is an open clock line.
(setq org-clock-in-resume t)

; Clear out zero-time clocks.
(setq org-clock-out-remove-zero-time-clocks t)

; Keep the clock running on the remember item.
(setq org-remember-clock-out-on-exit nil)

;; Don't clock out when moving task to a done state.
(setq org-clock-out-when-done nil)

(setq org-clock-modeline-total 'current)

; Timestamp done TODO items.
(setq org-log-done t)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; Make latex preview white on black to match the color theme.

; XXX: Color theme tends to vary, this assumes it's always black background.
; Values should be picked from current color theme.
(setq org-format-latex-options
      '(:foreground "White" :background "Black" :scale 1.2
        :matchers ("begin" "$" "$$" "\\(" "\\[")))

; Require braces for subscripts and superscripts, fixes the annoying
; subscript-itis from underscores in regular text.
(setq org-export-with-sub-superscripts '{})

; Setup the sequence of org-mode todo keywords.
;
; TODO: Tasks you know how to finish, can be finished in a single day and you
; are actively committed to finish.
;
; NEXT: Imminent tasks, things that you will work on today.
;
; FIXME: Broken things that need fixing.
;
; STARTED: Tasks you have started working on or tasks that are always ongoing
; such as following the news and answering email.
;
; WAITING: Tasks that can't be started before something else happens. Should
; explain what they're waiting on in the task text.
;
; SOMEDAY: Tasks you aren't actively committed to finish them. "I'll do it
; someday, maybe."
;
; PROJECT: Tasks you are committed to doing, but are too big or vague to be
; TODO items.
;
; CANCELED: Canceled tasks. Should explain why the task was canceled.
;
; DONE: Finished tasks.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
        (sequence "WAITING(w@)" "SOMEDAY(S)" "PROJECT(P)" "FIXME(f)" "|" "CANCELED(C@)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "chartreuse" :weight bold)
        ("FIXME" :foreground "violet red" :weight bold)
        ("NEXT" :foreground "orange" :weight bold)
        ("STARTED" :foreground "black" :background "green yellow" :weight bold)
        ("DONE" :foreground "slate gray" :weight bold)
        ("WAITING" :foreground "indian red" :weight bold)
        ("SOMEDAY" :foreground "medium orchid" :weight bold)
        ("PROJECT" :foreground "turquoise" :weight bold)
        ("CANCELED" :foreground "steel blue" :weight bold)))

; State triggers
;
; We want CANCELED and WAITING states to show up in subtasks as well. Do this
; by assigning tags to the tasks on setting the state.
(setq org-todo-state-tags-triggers
      '(("CANCELED" ("CANCELED" . t))
        ("WAITING" ("WAITING" . t) ("WORKINGON"))
        ("SOMEDAY" ("WAITING" . t))
        (done ("WORKINGON") ("WAITING"))
        ("TODO" ("WAITING") ("CANCELED"))
        ("FIXME" ("WAITING") ("CANCELED"))
        ("STARTED" ("WAITING") ("CANCELED"))
        ("PROJECT" ("CANCELED") ("PROJECT" . t))))

; Quick tags, add with C-c C-q
;
; WORKINGON is a tag for projects that are currently at top priority and from
; which the next task should be picked from.
(setq org-tag-alist '(("WORKINGON" . ?a)
                      ("WAITING" . ?w)
                      ("REFILE" . ?r)))

; Custom agenda
(setq org-agenda-custom-commands
      '(("p" "Active projects" tags "WORKINGON/PROJECT" ((org-use-tag-inheritance nil)))
        ("P" "All projects" tags "/PROJECT" ((org-use-tag-inheritance nil)))
        ("w" "Tasks waiting on something" tags "WAITING"
         ((org-use-tag-inheritance nil)))
        ("t" "Actively developed tasks" tags
        "WORKINGON/PROJECT|TODO|NEXT|STARTED|FIXME"
         ((org-use-tag-inheritance nil)))
        ("T" "Actively developed task subtrees" tags
        "WORKINGON/PROJECT|TODO|NEXT|STARTED|FIXME"
         ())
        ("n" "Started and upcoming tasks" tags "/NEXT|STARTED"
         ((org-agenda-todo-ignore-with-date nil)))
        ))

; Define stuck projects as PROJECT-tag trees ones without any TODOs or started
; tasks.
(setq org-stuck-projects '("/PROJECT-DONE" ("STARTED" "TODO" "NEXT" "FIXME") nil ""))

; Support for task effort estimates

; Do effort estimates by going into column mode with C-c C-x C-c and choosing
; a value in the Effort field.

; Set up the effort value for column-mode view.
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; Set up predefined effort values.
(setq org-global-properties '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

; Appointments from org agenda

; Rebuild reminders whenever agenda is modified.
; XXX: Clears all appts set via other means.
(defun regenerate-org-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(regenerate-org-appt)

(add-hook 'org-finalize-agenda-hook 'regenerate-org-appt)

(appt-activate t)

; Bring up the next day's appointments after midnight.
(run-at-time "24:01" nil 'regenerate-org-appt)

; Org and remember mode
(require 'remember)
(org-remember-insinuate)

; Key binding for doing org-remember
; XXX: Clobbers regexp-search backwards.
(global-set-key (kbd "C-M-r") (lambda () (interactive) (org-remember nil "n")))

; Collection bins for notes and tasks entered via remember.
; You can add the line
; #+FILETAGS: REFILE
; in the collection files to mark the entries as ones that should be refiled
; to more proper locations in due course.

; Put remember files in home or work subdir depending on which exists.
(let* ((prefix
        (cond ((file-exists-p "~/work/orghome") "~/work/orghome/")
              ((file-exists-p "~/work/orgwork") "~/work/orgwork/")
              (t "~/org/")))

       (notes-file (concat prefix "notes.org")))

  (setq org-remember-templates `((?w "* WWW: %:description\n  %T\n  [[%:link]]\n\n%i" ,notes-file "Notes" nil)
                                 ("note" ?n "* %?\n  %T" ,notes-file "Notes" nil))))

; Refiling settings

(setq org-completion-use-ido t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
(setq org-refile-use-outline-path (quote file))

; Latex settings

; Get \Perp symbol work for stochastic text.
(setq org-export-latex-append-header "\\newcommand{\\Perp}{\\perp \\! \\! \\! \\perp}")


(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes::%s]]" (reftex-citation t))))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)