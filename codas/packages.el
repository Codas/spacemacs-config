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
    ))

(defun codas/init-windmove () "")
(defun codas/init-slim-mode () "")
(defun codas/init-qml-mode () "")
(defun codas/init-exec-path-from-shell () "")
(defun codas/init-flyspell () "")
(defun codas/init-outline-magic () "")

(defun codas/init-ido-ubiquitous ()
  ""
  (use-package ido-ubiquitous
    :init
    (progn
      (ido-ubiquitous-mode +1)
      )
    )
  )

(defun codas/init-framemove ()
  ""
  (require 'framemove)
  (use-package framemove
    :init
    (progn
      (when (fboundp 'windmove-default-keybindings)
        (windmove-default-keybindings))
      (windmove-default-keybindings)
      (setq framemove-hook-into-windmove t)
      ))
  )

;;; packages.el ends here
