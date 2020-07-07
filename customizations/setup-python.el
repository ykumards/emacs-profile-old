(elpy-enable)

;; Flycheck for python syntax checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Force elpy to use ipython instead of
;; default interpreter
;; (elpy-use-ipython)

;; Correst PEP8 errors on save
(require 'py-autopep8)

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Make eply to use ipython by default
;;(elpy-use-ipython "ipython3")
;; Rope probably causes the memory leak
(setq elpy-rpc-backend "jedi")


