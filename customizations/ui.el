;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

(setq mac-command-modifier 'control)
(which-key-mode)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if (boundp 'fringe-mode)
  (fringe-mode '(4 . 0)))
(global-git-gutter-mode +1)

;; Configure the mode line
(setq-default mode-line-format
              '((:eval (propertize " %b" 'face 'mode-line-highlight))
                (:eval (list (nyan-create)))
                " %l:%c %p %m"))

;; typography
(setq-default line-spacing nil)
(setq mac-allow-anti-aliasing t)
(global-prettify-symbols-mode t)
(centaur-tabs-mode t)

;; Increase size for my poor eyes
(set-face-attribute 'default nil :font "Fira Code-16")

;; Better scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
(if (or (equal system-name "DH.local")
        (equal system-name "waffles"))
    (setq initial-frame-alist '((top . 0) (left . 0)
                                (width . 177) (height . 53)))
  (setq initial-frame-alist '((top . 0) (left . 0)
                              (width . 177) (height . 52))))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; open files in existing frame
(setq ns-pop-up-frames nil)


;; Show line numbers everywhere
(global-linum-mode)

;; Nyan-mode
(nyan-mode)

;; Line wraping
(global-visual-line-mode t)

;; lose the menubar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; COLOR THEMES
(setq heaven-and-hell-theme-type 'dark)
;; Set preferred light and dark themes
;; default light is emacs default theme, default dark is wombat
;; Themes can be the list: (dark . (tsdh-dark tango-dark))
(setq heaven-and-hell-themes
      '((light . sanityinc-solarized-light)
        (dark . doom-one)))

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)
