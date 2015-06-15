;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Configuration Layers
;; --------------------
(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-contrib/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(;; languages
                                       (haskell :variables
                                                haskell-enable-hindent-style "chris-done"
                                                haskell-enable-shm-support t)
                                       ruby
                                       html
                                       markdown
                                       javascript
                                       scala
                                       auctex
                                       emacs-lisp
                                       ;; utility
                                       (shell :variables
                                              shell-default-shell 'eshell)
                                       org
                                       org-repo-todo
                                       restclient
                                       smex
                                       colors
                                       git
                                       ;; user config file
                                       codas
                                       evil-little-word
                                       quickrun
                                       ;; spacemacs layer
                                       (auto-completion :variables
                                                        auto-completion-enable-sort-by-usage t)
                                       ;; better-defaults
                                       syntax-checking
                                       )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(flycheck-haskell
                                    company-ghc
                                    ghc
                                    helm-projectile)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t
   ))


;; Initialization Hooks
;; --------------------

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."

  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. If the value is an integer then the
   ;; banner with the corresponding index is used, if the value is `random'
   ;; then the banner is chosen randomly among the available banners, if
   ;; the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'random
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido t
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.5
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to nil
   ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; Enable micro-state for helm buffer when pressing on TAB."
   dotspacemacs-helm-micro-state t
   ;; If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame when it's active or selected. Transparency can
   ;; be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame when it's inactive or deselected. Transparency
   ;; can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 100
   ;; If non nil unicode symbols are displayed in the mode line (e.g. for lighters)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
   ;; overrides the default behavior of Emacs which recenters the point when
   ;; it reaches the top or bottom of the screen
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil

   ;; --------------------
   ;; non-spacemacs config
   ;; --------------------

   ;; limit line length tl 80 instead or 70
   fill-column 80
   whitespace-line-column 80
   ;; Use rvm to manage ruby version
   ruby-version-manager 'rvm
   ;; Enable projectile-rails
   ruby-on-rails-support t
   ns-use-native-fullscreen nil
   )

  ;; Use emacs 24.4 desktop save mode
  ;; Restores buffer and frames on emacs start to the previous configuration
  (desktop-save-mode t)
  ;; Make cabal bin directory visible to emacs (used by haskell-mode etc.)
  (add-to-list 'exec-path "~/.cabal/bin/")

  (add-to-load-path "~/.emacs.d/private/ide-backend-mode/")
  (add-to-load-path "~/.emacs.d/private/company-ide-backend/")
  (add-to-load-path "~/.emacs.d/private/flycheck-ide-backend/")

  ;; (autoload 'haskell-indentation-enable-show-indentations "haskell-indentation")
  ;; (autoload 'haskell-indentation-disable-show-indentations "haskell-indentation")

  ;; don't use fish in emacs shells
  (setq explicit-shell-file-name "/bin/zsh")
  (setq shell-file-name "/bin/zsh")
  )

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; use same OS X key configurations
  (setq mac-command-modifier 'meta mac-option-modifier nil)
  (setq evil-want-fine-undo nil)
  ;; Sane cursor and window movements
  (setq scroll-preserve-screen-position 1
        smooth-scroll-margin 3
        scroll-margin 4)

  ;; not needed (just visual select the line) and interferes with shm-mode
  (global-hl-line-mode -1)

  (setq-default js2-basic-offset 2
                css-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2)

  (use-package company
    :defer t
    :init
    (progn
      (setq company-frontends '(company-pseudo-tooltip-frontend
                                company-echo-metadata-frontend))))

  ;; disable helm for some commands
  ;; (use-package helm
  ;;   :defer t
  ;;   :config
  ;;   (progn
  ;;     (add-to-list 'helm-completing-read-handlers-alist '(describe-variable . ido))
  ;;     (add-to-list 'helm-completing-read-handlers-alist '(TeX-command-master . ido))
  ;;     ))

  ;; get these buffers out of the way!
  (push '("*GHC Info*" :noselect t) popwin:special-display-config)
  (push '("*Warnings*" :noselect t) popwin:special-display-config)

  (when (configuration-layer/layer-usedp 'syntax-checking)
    (setq-default flycheck-disabled-checkers '(haskell-ghc javascript-jshint)))

  (when (configuration-layer/layer-usedp 'auto-completion)
    (use-package web-mode
      :defer t
      :init
      (progn
        (add-hook 'scss-mode 'smartparens-mode)
        )
      )
    )

  ;; haskell config
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (evil-define-key 'normal haskell-mode-map ")" 'shm/close-paren)
      (evil-define-key 'insert haskell-mode-map (kbd "RET") 'shm/newline-indent)
      (evil-leader/set-key-for-mode 'haskell-mode
        "mht" 'ide-backend-mode-type
        "mt" 'ide-backend-mode-type ;; mht is really not that nice to type.
        "ml" 'ide-backend-mode-load))
    :config
    (progn
      (add-hook 'haskell-mode-hook 'custom-haskell-mode-hook)
      (require 'ide-backend-mode)
      (require 'company-ide-backend)
      (add-hook 'haskell-mode-hook 'ide-backend-mode)
      (spacemacs|add-company-hook haskell-mode)
      (push '(company-ide-backend) company-backends-haskell-mode)
      (when (configuration-layer/layer-usedp 'syntax-checking)
        (require 'flycheck-ide-backend)
        (flycheck-ide-backend-setup)
        )
      ))
  ;; latex
  (use-package tex
    :defer t
    :config (progn
              (add-hook 'LaTeX-mode-hook 'custom-latex-mode-hook)
              (progn
                (require 'texmathp)
                (evil-define-key
                  'visual LaTeX-mode-map (kbd "gw") 'LaTeX-fill-region)
                (push '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
                      TeX-command-list)
                (setq TeX-auto-save t
                      TeX-parse-self t
                      reftex-toc-split-windows-horizontally t
                      reftex-toc-split-windows-fraction 0.35
                      font-latex-fontify-script nil
                      font-latex-fontify-sectioning 'color
                      TeX-view-program-selection
                      '((output-pdf "PDF Viewer"))
                      TeX-view-program-list
                      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
                (setq-default TeX-master nil TeX-PDF-mode t)
                (evil-set-initial-state 'reftex-toc-mode 'normal)))))

(defun custom-latex-mode-hook ()
  (latex-math-mode)
  (turn-on-smartparens-mode)
  (setq TeX-command-default "latexmk")
  (outline-minor-mode)
  (TeX-source-correlate-mode)
  (auto-fill-mode)
  (flyspell-mode 1)
  )


(defun custom-haskell-mode-hook ()
  "Execute as haskell-mode-hook."
  (subword-mode +1)
  (custom-set-variables
   '(haskell-process-type 'cabal-repl)
   '(haskell-process-args-cabal-repl
     '("--ghc-option=-ferror-spans"))
   '(haskell-notify-p t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-use-presentation-mode t)
   '(haskell-process-reload-with-fbytecode nil)
   '(haskell-process-path-ghci "ghci-ng")
   '(haskell-process-args-ghci '("-ferror-spans"))
   '(haskell-process-args-cabal-repl
     '("--ghc-option=-ferror-spans"))
   '(haskell-process-generate-tags nil)
   '(haskell-process-log t)
   '(haskell-interactive-mode-eval-pretty nil)
   '(haskell-interactive-mode-eval-mode 'haskell-mode)
   '(shm-use-presentation-mode t)
   '(shm-auto-insert-skeletons t)
   )
  (setq haskell-complete-module-preferred
        '("Data.ByteString"
          "Data.ByteString.Lazy"
          "Data.Conduit"
          "Data.Function"
          "Data.List"
          "Data.Map"
          "Data.Maybe"
          "Data.Monoid"
          "Data.Ord"))
  )


;; Custom variables
;; ----------------

;; Do not write anything in this section. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(company-tooltip-align-annotations t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
 '(electric-indent-mode nil)
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#073642" t)
 '(flycheck-disabled-checkers (quote (haskell-ghc javascript-jshint)))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-popup-error nil t)
 '(haskell-notify-p t)
 '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
 '(haskell-process-args-ghci (quote ("-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci-ng")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(js2-basic-offset 2 t)
 '(js2-include-node-externs t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy scala-indent:operator-strategy)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
