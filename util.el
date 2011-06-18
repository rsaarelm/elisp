; Whether two strings are equal up to the length of the shortest string.
(defun has-prefix (prefix string) (eq (string-match prefix string) 0))

(defun current-year () (format-time-string "%Y"))

(defun roll ()
  "Roll dice interactively."
  (interactive)
  (let* ((dice-str (split-string (read-string "Roll ?d? ") "d"))
         (n (string-to-int (car dice-str)))
         (d (string-to-int (cadr dice-str)))
         (res 0))
    (while (> n 0)
      (setq res (+ res (+ 1 (random d))))
      (setq n (- n 1)))
    (print res)))

(defun join-strings (seq &optional separator)
  "Join a sequence of strings into one string with a separator."
  (if seq
      (if (cdr seq) ; If more than one item, put the separator between them.
          (concat (car seq) separator (join-strings (cdr seq) separator))
        (car seq))
    ""))

(defun prefix-lines-with (prefix text)
  (let* ((lines (split-string text "\n"))
         (prefixed-lines (map 'list (lambda (line) (concat prefix line)) lines)))
    (join-strings prefixed-lines "\n")))

(defun cpp-comment (text)
  "Convert a text block into a C++ style comment."
  (prefix-lines-with "// " text))

(defun scheme-comment (text)
  "Convert a text block into a Scheme comment."
  (prefix-lines-with "; " text))

(defun lua-comment (text)
  "Convert a text block into a Lua comment."
  (prefix-lines-with "-- " text))

(defun non-num-stringp (str) (or (equal str "") (equal str "n/a")))

(defun hh-mm-ss-to-seconds (time-string)
  (interactive)
  "Parse a string hh:mm:ss into seconds."
  (if (non-num-stringp time-string) ""
    (let* ((nums (map 'list #'string-to-number (split-string time-string ":")))
           (hours (car nums))
           (mins (cadr nums))
           (secs (caddr nums)))
      (+ (* hours 3600) (* mins 60) secs))))

(defun spreadsheet-string-to-number (number-string)
  (interactive)
  (if (non-num-stringp number-string) ""
    (float (string-to-number number-string))))

(defun spreadsheet-running-speed (hh-mm-ss-string km-string)
  (interactive)
  (let ((km (spreadsheet-string-to-number km-string))
        (sec (hh-mm-ss-to-seconds hh-mm-ss-string)))
    (if (or (non-num-stringp km) (non-num-stringp sec))
        ""
      (format "%.2f" (/ km (/ sec 3600.0))))))

(defun spreadsheet-running-meters-per-beat (hh-mm-ss-string bpm-string km-string)
  (interactive)
  (let ((km (spreadsheet-string-to-number km-string))
        (bpm (spreadsheet-string-to-number bpm-string))
        (sec (hh-mm-ss-to-seconds hh-mm-ss-string)))
    (if (or (non-num-stringp km) (non-num-stringp bpm) (non-num-stringp sec))
        ""
      (format "%.2f" (/ (* km 1000.0) (* bpm (/ sec 60.0)))))))

(defvar seconds-in-day (* 24 60 60))

(defun seconds-after-midnight (&optional time)
  (multiple-value-bind (sec min hour) (decode-time time)
    (+ (* 3600 hour) (* 60 min) sec)))

(defun delta-seconds (delta-sec &optional time)
  "Return a time value with delta-sec added to the seconds of the
given time value or current time."
  (let ((date (decode-time time)))
    (setcar date (+ (car date) delta-sec))
    (apply 'encode-time date)))

(defun next-daily-time (seconds-after-midnight &optional time)
  "When is the next occurrence of the given daily time point after given time."
  (let ((current-secs (seconds-after-midnight time)))
    (if (< current-secs seconds-after-midnight)
        (delta-seconds (- seconds-after-midnight current-secs) time)
      (delta-seconds
       (+ seconds-after-midnight (- seconds-in-day current-secs))
       time))))

(defun prev-daily-time (seconds-after-midnight &optional time)
  "When is the previous occurrence of the given daily time point after given time."
  (delta-seconds (- seconds-in-day) (next-daily-time seconds-after-midnight time)))

(defun load-elisp-directory (directory)
  (interactive)
  (if (file-directory-p directory)
      (mapc #'load (directory-files directory t "\\.el$"))))