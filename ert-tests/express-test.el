
;;; requires and setup

(when load-file-name
  (setq package-enable-at-startup nil)
  (setq package-load-list '((list-utils t)
                            (string-utils t)
                            (notify t)
                            (popup t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'list-utils)
(require 'string-utils)
(require 'notify)
(require 'popup)

(require 'express)


;;; express--message-insert-1

(ert-deftest express--message-insert-1-01 nil
  (should (equal "word\nmessage\n"
                 (with-temp-buffer
                   (insert "word")
                   (express--message-insert-1 "message")
                   (substring-no-properties (buffer-string))))))

(ert-deftest express--message-insert-1-02 nil
  (should (equal "line\nmessage\n"
                 (with-temp-buffer
                   (insert "line\n")
                   (express--message-insert-1 "message")
                   (substring-no-properties (buffer-string))))))


;;; express-install-aliases

(ert-deftest express-install-aliases-01 nil
  (express-install-aliases)
  (should (fboundp 'message-nolog)))

(ert-deftest express-install-aliases-02 nil
  (express-install-aliases -1)
  (should-not (fboundp 'message-nolog)))

(ert-deftest express-install-aliases-03 nil
  (express-install-aliases)
  (should (fboundp 'message-nolog)))


;;; express-message-noformat

(ert-deftest express-message-noformat-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an unformatted message, which should be \"hello%s\", including a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (express-message-noformat "hello%s" 'other 'args 'ignored)
     (should (equal "hello%s" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-noformat-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an unformatted message, which should be \"hello 2 %s\", including a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (express-with-message-noformat
       (message "hello 2 %s" 'other 'args 'ignored))
     (should (equal "hello 2 %s" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; express-message-maybe-formatted

(ert-deftest express-message-maybe-formatted-01 nil
  :tags '(:interactive)
  (should
   (let ((express-message-preformatted t)
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unformatted message, which should contain a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (express-message-maybe-formatted "message-maybe-formatted %s" 875)
     (should (equal "message-maybe-formatted %s" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-maybe-formatted-02 nil
  :tags '(:interactive)
  (should
   (let ((express-message-preformatted nil)
         (cursor-in-echo-area t))
     (read-char "Press a key to generate a formatted message, which should NOT contain a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (express-message-maybe-formatted "message-maybe-formatted %s" 875)
     (should (equal "message-maybe-formatted 875" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; express-message-logonly

(ert-deftest express-message-logonly-01 nil
  (let ((msg "message-logonly-123456"))
    (express-message-logonly msg)
    (should-not (equal msg (current-message)))))

(ert-deftest express-message-logonly-02 nil
  (let ((msg "message-logonly-123456"))
    (express-message-logonly msg)
    (should (equal msg
                   (with-current-buffer "*Messages*"
                     (goto-char (point-max))
                     (forward-line -1)
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

(ert-deftest express-message-logonly-03 nil
  (express-message-logonly "message-logonly-%s" 345678)
  (should (equal "message-logonly-345678"
                 (with-current-buffer "*Messages*"
                   (goto-char (point-max))
                   (forward-line -1)
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(ert-deftest express-message-logonly-04 nil
  (let ((msg "message-logonly-98765"))
    (express-with-message-logonly
      (message msg))
    (should-not (equal msg (current-message)))
    (should (equal msg
                   (with-current-buffer "*Messages*"
                     (goto-char (point-max))
                     (forward-line -1)
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))


;;; express-message-insert

(ert-deftest express-message-insert-01 nil
  (let ((msg "message-insert-123456"))
    (with-temp-buffer
      (express-message-insert msg)
      (should-not (equal msg (current-message))))))

(ert-deftest express-message-insert-02 nil
  (let ((msg "message-insert-123456"))
    (with-temp-buffer
      (express-message-insert msg)
      (should (equal msg
                     (with-current-buffer "*Messages*"
                       (goto-char (point-max))
                       (forward-line -1)
                       (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))))

(ert-deftest express-message-insert-03 nil
  (with-temp-buffer
    (express-message-insert "message-insert-%s" 345678)
    (should (equal "message-insert-345678"
                   (progn
                     (forward-line -1)
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

(ert-deftest express-message-insert-04 nil
  (let ((msg "message-insert-314159"))
    (with-temp-buffer
      (express-with-message-insert
        (message "message-insert-314159"))
      (should-not (equal msg (current-message)))
      (should (equal msg
                     (progn
                       (forward-line -1)
                       (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))))


;;; express-message-string

(ert-deftest express-message-string-01 nil
  (let ((msg "message-string-123456"))
    (express-message-string msg)
    (should-not (equal msg (current-message)))))

(ert-deftest express-message-string-02 nil
  (let ((msg "message-string-123456")
        (str-val nil))
    (setq str-val (express-message-string msg))
    (should (equal (concat msg "\n") str-val))))

(ert-deftest express-message-string-03 nil
  (let ((str-val nil))
    (setq str-val (express-message-string "message-string-%s" 345678))
    (should (equal "message-string-345678\n" str-val))))

(ert-deftest express-message-string-04 nil
  (let ((msg "message-string-314159")
        (str-val nil))
    (setq str-val
          (express-with-message-string
            (message msg)
            (message msg)))
    (should (equal (concat msg "\n" msg "\n") str-val))))


;;; express-message-nolog

(ert-deftest express-message-nolog-01 nil
  :tags '(:interactive)
  (should
   (let ((msg "message-nolog-123456")
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (express-message-nolog msg)
     (should (equal msg (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-nolog-02 nil
  :tags '(:interactive)
  (should
   (let ((msg "message-nolog-123456")
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (express-message-nolog msg)
     (should-not (equal msg
                        (with-current-buffer "*Messages*"
                          (goto-char (point-max))
                          (forward-line -1)
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-nolog-03 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (express-message-nolog "message-nolog-%s" 345678)
     (should (equal "message-nolog-345678" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-nolog-04 nil
  :tags '(:interactive)
  (should
   (let ((msg "message-nolog-314159")
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (express-with-message-nolog
       (message msg))
     (should (equal msg (current-message)))
     (should-not (equal msg
                        (with-current-buffer "*Messages*"
                          (goto-char (point-max))
                          (forward-line -1)
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; express-message-temp

(ert-deftest express-message-temp-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t)
         (express-message-seconds 1))
     (read-char "Press a key to generate a temporary message which disappears, leaving a permanent message")
     (setq cursor-in-echo-area nil)
     (message "permanent message")
     (express-message-temp "disappearing message")
     (sleep-for 2)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-temp-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t)
         (express-message-seconds 1))
     (read-char "Press a key to generate a temporary message which disappears, leaving a permanent message")
     (setq cursor-in-echo-area nil)
     (message "permanent message 2")
     (express-with-message-temp
       (message "disappearing message 2"))
     (sleep-for 2)
     (y-or-n-p "Did that work as expected?"))))


;;; express-message-popup

;; worse than failure to appear, this sometimes seems like
;; a wedge to the user, y-or-n-p prompt does not appear
;;
;; (ert-deftest express-message-popup-01 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate a popup message")
;;      (setq cursor-in-echo-area nil)
;;      (express-message-popup "popup message 1")
;;      (sleep-for 1)
;;      (y-or-n-p "Did that work as expected?"))))
;;
;; (ert-deftest express-message-popup-02 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate a popup message")
;;      (setq cursor-in-echo-area nil)
;;      (express-with-message-popup
;;        (message "popup message 2"))
;;      (sleep-for 1)
;;      (y-or-n-p "Did that work as expected?"))))


;;; express-message-notify

(ert-deftest express-message-notify-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a notification message")
     (setq cursor-in-echo-area nil)
     (express-message-notify "notification message 1")
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-notify-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a notification message")
     (setq cursor-in-echo-area nil)
     (express-with-message-notify
       (message "notification message 1"))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; express-message-highlight

(ert-deftest express-message-highlight-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a highlighted message")
     (setq cursor-in-echo-area nil)
     (express-message-highlight "highlighted message 1")
     (sleep-for 1)
     (with-current-buffer "*Messages*"
       (goto-char (point-max))
       (forward-line -1)
       (should (equal-including-properties
                (buffer-substring               (line-beginning-position) (line-end-position))
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-message-highlight-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a highlighted message")
     (setq cursor-in-echo-area nil)
     (express-with-message-highlight
       (message "highlighted message 2"))
     (sleep-for 1)
     (with-current-buffer "*Messages*"
       (goto-char (point-max))
       (forward-line -1)
       (should (equal-including-properties
                (buffer-substring               (line-beginning-position) (line-end-position))
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (y-or-n-p "Did that work as expected?"))))


;;; express - todo more should assertions are possible to check state after expressmsgs

(ert-deftest express-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a standard express msg")
     (setq cursor-in-echo-area nil)
     (express "standard express msg")
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg without sound")
     (setq cursor-in-echo-area nil)
     (express "express msg without sound" 'quiet)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-03 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which persists for five seconds")
     (setq cursor-in-echo-area nil)
     (express "express msg for five seconds" nil 5)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-04 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg without a timeout (will disappear quickly)")
     (setq cursor-in-echo-area nil)
     (express "express msg without timeout" nil 0)
     (sleep-for .5)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-05 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg without color")
     (setq cursor-in-echo-area nil)
     (express "express msg without color" nil nil 'nocolor)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-06 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which is logged")
     (setq cursor-in-echo-area nil)
     (express "express msg which is logged" nil nil nil t)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-07 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which is only logged, not visible")
     (setq cursor-in-echo-area nil)
     (express "express msg which is only logged" nil nil nil 'log-only)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-08 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which also appears as a notification")
     (setq cursor-in-echo-area nil)
     (express "express msg in echo area and notification" nil nil nil nil t)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-09 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which only appears as a notification")
     (setq cursor-in-echo-area nil)
     (express "express msg as notification, no echo area" nil nil nil nil 'replace-echo)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-10 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which simulates `message'")
     (setq cursor-in-echo-area nil)
     (express (format "%s message" "standard") 'quiet 0 'nocolor 'log)
     (sleep-for 1)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

;; worse than failure, these may appear to wedge
;;
;; (ert-deftest express-11 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate an express msg which also appears as a popup")
;;      (setq cursor-in-echo-area nil)
;;      (express "express msg in echo area and popup" nil nil nil nil nil t)
;;      (setq cursor-in-echo-area t)
;;      (y-or-n-p "Did that work as expected?"))))
;;
;; (ert-deftest express-12 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate an express msg which only appears as a popup")
;;      (setq cursor-in-echo-area nil)
;;      (express "express msg as popup, no echo area" nil nil nil nil nil 'replace-echo)
;;      (setq cursor-in-echo-area t)
;;      (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-13 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which stringifies a number")
     (setq cursor-in-echo-area nil)
     (express 3.14159)
     (sleep-for 1)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest express-14 nil
  "Test CL form"
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an express msg which appears without color")
     (setq cursor-in-echo-area nil)
     (express* "express msg without color" :nocolor t)
     (sleep-for 1)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))


;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; express-test.el ends here
