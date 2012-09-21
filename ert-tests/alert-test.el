
;; requires and setup

(when load-file-name
  (setq package-enable-at-startup nil)
  (setq package-load-list '((string-utils t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'string-utils)
(require 'alert)

;;; this is a stub - no tests defined

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
