;;; email/mutt/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist
             '("/tmp/neomutt-" . mail-mode))

(defun dmr:mail-mode-hook ()
  (setq fill-column 60))

(add-hook! 'mail-mode-hook #'dmr:mail-mode-hook)
