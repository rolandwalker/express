
;; requires and setup

(when load-file-name
  (setq package-enable-at-startup nil)
  (setq package-load-list '((string-utils t)
                            (notify t)
                            (popup t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'string-utils)
(require 'notify)
(require 'popup)

(require 'alert)

;;; alert--message-insert-1

(ert-deftest alert--message-insert-1-01 nil
  (should (equal "word\nmessage\n"
                 (with-temp-buffer
                   (insert "word")
                   (alert--message-insert-1 "message")
                   (substring-no-properties (buffer-string))))))

(ert-deftest alert--message-insert-1-02 nil
  (should (equal "line\nmessage\n"
                 (with-temp-buffer
                   (insert "line\n")
                   (alert--message-insert-1 "message")
                   (substring-no-properties (buffer-string))))))


;;; alert-install-aliases

(ert-deftest alert-install-aliases-01 nil
  (alert-install-aliases)
  (should (fboundp 'message-nolog)))

(ert-deftest alert-install-aliases-02 nil
  (alert-install-aliases -1)
  (should-not (fboundp 'message-nolog)))

(ert-deftest alert-install-aliases-03 nil
  (alert-install-aliases)
  (should (fboundp 'message-nolog)))


;;; alert-message-noformat

(ert-deftest alert-message-noformat-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an unformatted message, which should be \"hello%s\", including a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (alert-message-noformat "hello%s" 'other 'args 'ignored)
     (should (equal "hello%s" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; alert-message-maybe-formatted

(ert-deftest alert-message-maybe-formatted-01 nil
  :tags '(:interactive)
  (should
   (let ((alert-message-preformatted t)
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unformatted message, which should contain a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (alert-message-maybe-formatted "message-maybe-formatted %s" 875)
     (should (equal "message-maybe-formatted %s" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-message-maybe-formatted-02 nil
  :tags '(:interactive)
  (should
   (let ((alert-message-preformatted nil)
         (cursor-in-echo-area t))
     (read-char "Press a key to generate a formatted message, which should NOT contain a literal percent sign.")
     (setq cursor-in-echo-area nil)
     (alert-message-maybe-formatted "message-maybe-formatted %s" 875)
     (should (equal "message-maybe-formatted 875" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; alert-message-logonly

(ert-deftest alert-message-logonly-01 nil
  (let ((msg "message-logonly-123456"))
    (alert-message-logonly msg)
    (should-not (equal msg (current-message)))))

(ert-deftest alert-message-logonly-02 nil
  (let ((msg "message-logonly-123456"))
    (alert-message-logonly msg)
    (should (equal msg
                   (with-current-buffer "*Messages*"
                     (goto-char (point-max))
                     (forward-line -1)
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

(ert-deftest alert-message-logonly-03 nil
  (alert-message-logonly "message-logonly-%s" 345678)
  (should (equal "message-logonly-345678"
                 (with-current-buffer "*Messages*"
                   (goto-char (point-max))
                   (forward-line -1)
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))


;;; alert-message-insert

(ert-deftest alert-message-insert-01 nil
  (let ((msg "message-insert-123456"))
    (with-temp-buffer
      (alert-message-insert msg)
      (should-not (equal msg (current-message))))))

(ert-deftest alert-message-insert-02 nil
  (let ((msg "message-insert-123456"))
    (with-temp-buffer
      (alert-message-insert msg)
      (should (equal msg
                     (with-current-buffer "*Messages*"
                       (goto-char (point-max))
                       (forward-line -1)
                       (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))))

(ert-deftest alert-message-insert-03 nil
  (with-temp-buffer
    (alert-message-insert "message-insert-%s" 345678)
    (should (equal "message-insert-345678"
                   (progn
                     (forward-line -1)
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))


;;; alert-message-nolog

(ert-deftest alert-message-nolog-01 nil
  :tags '(:interactive)
  (should
   (let ((msg "message-nolog-123456")
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (alert-message-nolog msg)
     (should (equal msg (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-message-nolog-02 nil
  :tags '(:interactive)
  (should
   (let ((msg "message-nolog-123456")
         (cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (alert-message-nolog msg)
     (should-not (equal msg
                        (with-current-buffer "*Messages*"
                          (goto-char (point-max))
                          (forward-line -1)
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-message-nolog-03 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an unlogged message")
     (setq cursor-in-echo-area nil)
     (alert-message-nolog "message-nolog-%s" 345678)
     (should (equal "message-nolog-345678" (current-message)))
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))


;;; alert-message-temp

(ert-deftest alert-message-temp-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t)
         (alert-message-seconds 1))
     (read-char "Press a key to generate a temporary message which disappears, leaving a permanent message")
     (setq cursor-in-echo-area nil)
     (message "permanent message")
     (alert-message-temp "disappearing message")
     (sleep-for 2)
     (y-or-n-p "Did that work as expected?"))))


;;; alert-message-popup

;; worse than failure to appear, this sometimes seems like
;; a wedge to the user, y-or-n-p prompt does not appear
;;
;; (ert-deftest alert-message-popup-01 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate a popup message")
;;      (setq cursor-in-echo-area nil)
;;      (alert-message-popup "popup message")
;;      (sleep-for 1)
;;      (y-or-n-p "Did that work as expected?"))))


;;; alert-message-notify

(ert-deftest alert-message-notify-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a notification message")
     (setq cursor-in-echo-area nil)
     (alert-message-notify "notification message")
     (sleep-for 1)
     (y-or-n-p "Did that work as expected?"))))

;;; alert-message-highlight

(ert-deftest alert-message-highlight-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a highlighted message")
     (setq cursor-in-echo-area nil)
     (alert-message-highlight "highlighted message")
     (sleep-for 1)
     (with-current-buffer "*Messages*"
       (goto-char (point-max))
       (forward-line -1)
       (should (equal-including-properties
                (buffer-substring               (line-beginning-position) (line-end-position))
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
     (y-or-n-p "Did that work as expected?"))))

;;; alert - todo more should assertions are possible to check state after alerts

(ert-deftest alert-01 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate a standard alert")
     (setq cursor-in-echo-area nil)
     (alert "standard alert")
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-02 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert without sound")
     (setq cursor-in-echo-area nil)
     (alert "alert without sound" 'quiet)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-03 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which persists for five seconds")
     (setq cursor-in-echo-area nil)
     (alert "alert for five seconds" nil 5)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-04 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert without a timeout (will disappear quickly)")
     (setq cursor-in-echo-area nil)
     (alert "alert without timeout" nil 0)
     (sleep-for .5)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-05 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert without color")
     (setq cursor-in-echo-area nil)
     (alert "alert without color" nil nil 'nocolor)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-06 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which is logged")
     (setq cursor-in-echo-area nil)
     (alert "alert which is logged" nil nil nil t)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-07 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which is only logged, not visible")
     (setq cursor-in-echo-area nil)
     (alert "alert which is only logged" nil nil nil 'log-only)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-08 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which also appears as a notification")
     (setq cursor-in-echo-area nil)
     (alert "alert in echo area and notification" nil nil nil nil t)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-09 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which only appears as a notification")
     (setq cursor-in-echo-area nil)
     (alert "alert as notification, no echo area" nil nil nil nil 'replace-echo)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

(ert-deftest alert-10 nil
  :tags '(:interactive)
  (should
   (let ((cursor-in-echo-area t))
     (read-char "Press a key to generate an alert which simulates `message'")
     (setq cursor-in-echo-area nil)
     (alert (format "%s message" "standard") 'quiet 0 'nocolor 'log)
     (sleep-for 1)
     (setq cursor-in-echo-area t)
     (y-or-n-p "Did that work as expected?"))))

;; worse than failure, these may appear to wedge
;;
;; (ert-deftest alert-11 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate an alert which also appears as a popup")
;;      (setq cursor-in-echo-area nil)
;;      (alert "alert in echo area and popup" nil nil nil nil nil t)
;;      (setq cursor-in-echo-area t)
;;      (y-or-n-p "Did that work as expected?"))))
;;
;; (ert-deftest alert-12 nil
;;   "Test Bug: Popups fail to appear under the test harness"
;;   :tags '(:interactive)
;;   :expected-result :failed
;;   (should
;;    (let ((cursor-in-echo-area t))
;;      (read-char "Press a key to generate an alert which only appears as a popup")
;;      (setq cursor-in-echo-area nil)
;;      (alert "alert as popup, no echo area" nil nil nil nil nil 'replace-echo)
;;      (setq cursor-in-echo-area t)
;;      (y-or-n-p "Did that work as expected?"))))

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

;;; alert-test.el ends here
