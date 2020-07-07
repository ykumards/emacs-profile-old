; Org agenda
(global-set-key (kbd "C-q") 'org-agenda)

;; Neotree toggle
;; (global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "C-C n") 'treemacs)

;;windmove
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Projectile Shortcuts
;; This gives us a sublime like shortcut to open a file
;; we can add the directory name along with the filename
(global-set-key (kbd "C-p")  'projectile-find-file)
(global-set-key (kbd "C-f") 'helm-projectile-grep)

;; Steve Yegge's idea to use replace M-x with C-x C-m
(global-set-key "\C-x\C-m" 'helm-M-x)
(global-set-key "\C-c\C-m" 'helm-M-x) 


(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Add shortcut for helm-org-rifle
(global-set-key "\C-cf" 'helm-org-rifle)
;; Helm projectile switch project
(global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)

(global-set-key (kbd "C-x C-g") 'deft-find-file)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

