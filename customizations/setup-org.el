;; The following lines are always needed.  Choose your own keys.

;; Prettify to display latex symbols inline
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)
;; (setq org-pretty-entities t
;;       org-pretty-entities-include-sub-superscripts t)
;; (setq org-toggle-pretty-entities 1)

;; (require 'org-super-agenda)
;; (let ((org-super-agenda-groups
;;        '((:log t)  ; Automatically named "Log"
;;          (:name "Schedule"
;;                 :time-grid t)
;;          (:name "Today"
;;                 :scheduled today)
;;          (:habit t)
;;          (:name "Due today"
;;                 :deadline today)
;;          (:name "Overdue"
;;                 :deadline past)
;;          (:name "Due soon"
;;                 :deadline future)
;;          (:name "Unimportant"
;;                 :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
;;                 :order 100)
;;          (:name "Waiting..."
;;                 :todo "WAITING"
;;                 :order 98)
;;          (:name "Scheduled earlier"
;;                 :scheduled past))))
;;   (org-agenda-list))

;;(setq org-agenda-files (directory-files-recursively "~/Documents/Work/org/" "\\.org$"))
(setq org-agenda-files (list "~/Documents/Work/org/2020.org"
                             "~/Documents/Work/org/todo.org"
                             "~/Documents/Work/org/inbox.org"
                             "~/Documents/Work/org/projects.org"))
(setq org-default-notes-file "~/Documents/Work/org/inbox.org")

(setq org-indent-mode t)
(setq org-startup-with-inline-images t)
(setq org-agenda-time-grid
        '((daily today require-timed)
          ()
           "......"
          "----------------------"
))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;; bullets
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("•"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "MEETING(m)" "|" "DONE(d)"))))

(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "blue" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; State change triggers add a tag to the task
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Handling refiling
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Lets use helm not IDO
(setq org-completion-use-ido nil)
(setq ido-everywhere nil)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/Work/org/inbox.org")
               "* TODO %?\n%U\n%a\n")
              ("j" "Journal Entry"
               entry (file+datetree "~/Documents/Work/org/journal.org")
               "* %?"
               :empty-lines 1)
              ("n" "note" entry (file "~/Documents/Work/org/inbox.org")
               "* %? :NOTE:\n%U\n%a\n"))))

;; Configure refile to use ido and allow nested targets
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq org-indent-indentation-per-level 2)
(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 5))))
(setq org-capture-bookmark nil)

;; (setq mode-line-format
;;       (:propertize
;;        (t org-mode-line-string)
;;        (:foreground "cyan" :weight 'bold)))

;; Org Journal
(use-package org-journal
  :bind
  ("C-c j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/Work/org/notes/musings")
  (org-journal-date-format "%A, %d %B %Y"))

;; Deft shortcuts
(setq deft-extensions '("org" "md" "txt" "rtf"))
(setq deft-directory "~/Documents/Work/org/notes")
(setq deft-recursive t)
(global-set-key "\C-cd" 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-org-mode-title-prefix t)
(setq deft-auto-save-interval nil)
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

(global-set-key (kbd "C-c r")
                (lambda () (interactive) (find-file "~/Documents/Work/org/inbox.org")))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Documents/Work/org/notes")
      :bind (:map org-roam-mode-map
              (("C-c h l" . org-roam)
               ("C-c h f" . org-roam-find-file)
               ("C-c h g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c h i" . org-roam-insert))
              (("C-c h I" . org-roam-insert-immediate))))
(setq org-roam-completion-system 'helm)

;; https://github.com/alhassy/org-special-block-extras#installation-instructions
;; (require 'org-special-block-extras)
;; (use-package org-special-block-extras
;;  :ensure t
;;  :hook (org-mode . org-special-block-extras-mode))
;; (add-hook #'org-mode-hook #'org-special-block-extras-mode)
