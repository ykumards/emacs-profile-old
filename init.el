;;;;
;; Straight Packages
;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install straight-use-package libraries permanently
(defvar straight-to-install
  '(
      ag
      autopair

      base16-theme
      paredit

      centaur-tabs
      crux
      deft
      treemacs

      evil
      ;; Python
      ein
      elpy
      py-autopep8
      flycheck

      projectile
      ;; git integration
      magit

      ;; ORG
      org
      org-journal
      org-roam
      org-bullets

      rainbow-delimiters

      ;; helm
      helm
      helm-ag
      helm-descbinds
      helm-projectile
      helm-org-rifle

      ido
      ido-ubiquitous

      git-gutter

      recentf
      smex

      yaml-mode
      nyan-mode
      which-key

      ;; Terminals
      vterm

      ;; Themes
      dracula-theme
      doom-themes
      color-theme-sanityinc-solarized

      heaven-and-hell
))

(dolist (package straight-to-install)
  (straight-use-package package))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")


;; These customizations make editing a bit nicer.
(load "editing.el")

;; Sets up shortcuts
(load "setup-shortcuts.el")

; ;; Sets up exec-path-from-shell so that Emacs will use the correct
; ;; environment variables
; (load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; ;; For editing lisps
;; ;; (load "elisp-editing.el")

;; helm
(load "setup-helm.el")

;; Langauage-specific
(load "setup-org.el")

;; Languages
(load "setup-python.el")
(load "setup-yaml.el")
(load "setup-js.el")

;; ;; treemacs config
;; (load "setup-treemacs.el")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [unspecified "#fdf6e3" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#586e75"] t)
 '(coffee-tab-width 2)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "73e35ffa5ca98b57a9923954f296c3854ce6d8736b31fdbdda3d27502d4b4d69" "77bddca0879cb3b0ecdf071d9635c818827c57d69164291cb27268ae324efa84" "554b7f0439155d6eb648d4837ef03902f51124cacee021217e76f39e9dd314c2" "0a3a41085c19d8121ed0ad3eb658a475ccb948a70a83604641ee7d4c3575a4d5" "a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "3481e594ae6866d72c40ad77d86a1ffa338d01daa9eb0977e324f365cef4f47c" "2e1d19424153d41462ad31144549efa41f55dacda9b76571f73904612b15fd0a" "0be964eabe93f09be5a943679ced8d98e08fe7a92b01bf24478e56eee7b6b21d" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8cb48d15dac4ac08bc97cff5a84554833a6140c8907f50d1e5de755f3b324b6d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "50b64810ed1c36dfb72d74a61ae08e5869edc554102f20e078b21f84209c08d1" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "f869a5d068a371532c82027cdf1feefdc5768757c78c48a7e0177e90651503ad" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(cycle-themes-mode t)
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(org-roam org-static-blog treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs org-super-agenda org-brain emacs-xkcd org-ac helm-org-rifle centaur-tabs pdf-tools org-pdftools multiple-cursors auctex pandoc-mode pandoc org-download worf heaven-and-hell magit-todos magit-find-file ivy-bibtex helm-bibtex zotxt ein-mumamo doom-themes atom-one-dark-theme slime fill-column-indicator dakrone-light-theme solarized-theme color-theme-solarized color-theme-sanityinc-solarized spacemacs-theme autopair counsel-dash ob-ipython company-lua lua-mode websocket ein deft geiser speed-type racket-mode py-autopep8 flycheck org-journal nyan-mode dashboard which-key elpy neotree org-bullets base16-theme clojure-mode scss-mode yaml-mode web-mode use-package thesaurus tagedit smex rainbow-delimiters php-mode noflet markdown-mode magit ido-ubiquitous helm-projectile helm-ls-git helm-git-grep helm-descbinds helm-ag haskell-mode haml-mode git-gutter exec-path-from-shell enh-ruby-mode dockerfile-mode csharp-mode crux company color-theme-sanityinc-tomorrow clj-refactor aggressive-indent ag adoc-mode adjust-parens ack-and-a-half))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values
   '((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-2 ((t (:foreground "#5fb3b3"))))
 '(org-level-4 ((t (:foreground "#ddc794")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
