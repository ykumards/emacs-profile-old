;;; heaven-and-hell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "heaven-and-hell" "heaven-and-hell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from heaven-and-hell.el

(autoload 'heaven-and-hell-toggle-theme "heaven-and-hell" "\
If `heaven-and-hell-theme-type' is `light' - load dark theme/s.
And vise-versa.

\(fn)" t nil)

(autoload 'heaven-and-hell-load-default-theme "heaven-and-hell" "\
Disable all custom themes e.g. load default Emacs theme.

\(fn)" t nil)

(autoload 'heaven-and-hell-init-hook "heaven-and-hell" "\
Add this to `after-init-hook' so it can load your theme/s of choice correctly.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "heaven-and-hell" '("heaven-and-hell-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; heaven-and-hell-autoloads.el ends here
