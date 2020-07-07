(use-package pdf-tools
 ;; :pin manual ;; manually update
 :config
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
;; disable global linum mode when pdf-tools is on as they have a conflict
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
