;;; config.el --- My (Codas) Layer config File for Spacemacs
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

(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-s") 'save-buffer)

(evil-leader/set-key
  "bl"  'switch-to-buffer)


(defun codas-yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'codas-yank-to-end-of-line)

;; (evil-leader/set-key
;;   "pf"  'projectile-find-file)

;;; config.el ends here
