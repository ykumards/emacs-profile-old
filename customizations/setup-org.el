;; The following lines are always needed.  Choose your own keys.
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
;; (add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

;; Original cycle agenda files is broken
;; (defun org-cycle-agenda-files ()
;;   "Cycle through the files in `org-agenda-files'.
;; If the current buffer visits an agenda file, find the next one in the list.
;; If the current buffer does not, find the first agenda file."
;;   (interactive)
;;   (let* ((fs (org-agenda-files t))
;;    (files (append fs (list (car fs))))
;;    (tcf (if buffer-file-name (file-truename buffer-file-name)))
;;    file)
;;     (unless files (user-error "No agenda files"))
;;     (catch 'exit
;;       (while (setq file (pop files))
;;   (if (equal (file-truename file) tcf)
;;       (when (car files)
;;         (find-file (car files))
;;         (throw 'exit t))))
;;       (find-file (car fs)))
;;     (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))))

;; Enable org-super-agenda
;; (setq org-super-agenda-mode t)

;; Prettify to display latex symbols inline
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)
;; (setq org-pretty-entities t
;;       org-pretty-entities-include-sub-superscripts t)
;; (setq org-toggle-pretty-entities 1)
(setq org-agenda-files (directory-files-recursively "~/Documents/Work/org/" "\\.org$"))
;; (setq org-agenda-files (list "~/Documents/Work/org/"))
;;                              "~/Documents/Work/"))
(setq org-default-notes-file "~/Documents/Work/org/inbox.org")

(setq org-indent-mode t)
(setq org-startup-with-inline-images t)

;; bullets
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("•"))

;; (define-key org-mode-map (kbd "$") (lambda ()
;;                                      (interactive)
;;                                      (insert "$")
;;                                      (save-excursion
;;                                        (left-char 1)
;;                                        (if (org-inside-LaTeX-fragment-p)
;;                                            (progn
;;                                              (right-char 2)
;;                                              (org-preview-latex-fragment))))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))

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


(setq org-startup-indented t)
(setq org-deadline-warning-days 3)

;; Configure refile to use ido and allow nested targets
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 5))))
(setq org-capture-bookmark nil)

;; (setq mode-line-format
;;       (:propertize
;;        (t org-mode-line-string)
;;        (:foreground "cyan" :weight 'bold)))

;; Org Journal
(require 'org-journal)
(setq org-journal-dir "~/Documents/Work/org/musings")
(setq org-journal-enable-encryption nil)
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-date-prefix "#+TITLE: Work Notes ")
(global-set-key "\C-cj" 'org-journal-new-entry)

;; Deft shortcuts
(setq deft-extensions '("org" "md" "txt" "rtf"))
(setq deft-directory "~/Documents/Work/org/notes")
(setq deft-recursive t)
(global-set-key "\C-cd" 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval nil)
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

(global-set-key (kbd "C-c r")
                (lambda () (interactive) (find-file "~/Documents/Work/org/inbox.org")))

;; https://github.com/alhassy/org-special-block-extras#installation-instructions
(use-package org-special-block-extras
  :ensure t
  :hook (org-mode . org-special-block-extras-mode))

(defun org-special-block-extras--foo (backend contents)
  "The FOO block type replaces all occurances of ‘foo’ with ‘bar’,
unless a ‘:replacement:’ is provided."
  (-let [(contents′ . (&alist 'replacement))
           (org-special-block-extras--extract-arguments contents 'replacement)]
    (s-replace "foo" (or replacement "bar") contents′)))

;; (add-hook #'org-mode-hook #'org-special-block-extras-mode)

