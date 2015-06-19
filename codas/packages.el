;;; packages.el --- My (Codas) Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2015 Arne Link
;;
;; Author: Arne Link <ling.arne@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; User Configuration file

;;; Code:
(defvar codas-packages
  '(
    framemove
    windmove
    slim-mode
    qml-mode
    exec-path-from-shell
    flyspell
    outline-magic
    ido-ubiquitous
    shakespeare-mode
    ))


(defun codas/init-shakespeare-mode ()
  (use-package shakespeare-hamlet-mode
    :defer t
    :init
    (progn
      (defun codas-shakespare-mode-hook ()
        (disable-electric-indent-mode)
        (smartparens-mode)
        )
      (add-hook 'shakespeare-hamlet-mode-hook 'codas-shakespare-mode-hook)
      )
    )
  )

(defun codas/init-windmove () "")
(defun codas/init-slim-mode () "")
(defun codas/init-qml-mode () "")
(defun codas/init-exec-path-from-shell () "")
(defun codas/init-flyspell () "")
(defun codas/init-outline-magic () "")

(defun codas/init-ido-ubiquitous ()
  ""
  (require 'ido-ubiquitous)
  (use-package ido-ubiquitous
    :config
    (progn
      (ido-ubiquitous-mode +1)
      )
    )
  )

(defun codas/init-framemove ()
  ""
  (require 'framemove)
  (use-package framemove
    :config
    (progn
      (when (fboundp 'windmove-default-keybindings)
        (windmove-default-keybindings))
      (windmove-default-keybindings)
      (setq framemove-hook-into-windmove t)
      ))
  )

;;; packages.el ends here
