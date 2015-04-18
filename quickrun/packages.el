;;; packages.el --- quickrun Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar quickrun-packages
  '(
    quickrun
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar quickrun-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function quickrun/init-<package-quickrun>
;;
(defun quickrun/init-quickrun ()
  "Initialize my package"
  (use-package quickrun
    :init
    (progn
      (defun quickrun-maybe-region ()
        ""
        (interactive)
        (cond
          ((eq evil-state 'visual)
           (quickrun-region (region-beginning) (region-end)))
          (t
           (quickrun))
          ))
      (push '("*quickrun*") popwin:special-display-config)
      (evil-define-key 'normal quickrun/mode-map
        "q" 'delete-window)
      ;; Add C++ command for C11 and set it default in C++ file.
      ;; (quickrun-add-command "haskell/cabal"
      ;;                       '((:command . "cabal")
      ;;                         (:exec    . ("exec -- runghc %c %o %s %a")))
      ;;                       :default "haskell")
      (evil-leader/set-key
        "cqq" 'quickrun-maybe-region
        "cqr" 'quickrun-replace-region
        "cqa" 'quickrun-with-arg
        "cqs" 'quickrun-shell)
      ))
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
