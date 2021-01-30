;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "dmr"
      user-mail-address "dmr@dmr.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (font-spec :family "gohufont" :size 14))

(setq doom-theme 'doom-one)

(setq org-directory "~/code/elisp/org-dir/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(global-set-key (kbd "C-;") 'ace-window)
(after! ace-window
        (set-face-attribute 'aw-leading-char-face nil
                            :foreground "green"
                            :family "Source Code Pro"
                            :weight 'bold
                            :height 2.0)
        (set-face-attribute 'aw-minibuffer-leading-char-face nil
                            :foreground "red"
                            :height 100)
        (setq aw-scope 'frame)
        (setq aw-ignore-current t))

(after! flycheck
        (add-to-list '+emacs-lisp-disable-flycheck-in-dirs
                     "~/code/elisp/framework/doom/doom-emacs"
                     "~/code/elisp/framework/doom/user-doomdir"))

;; dumb parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(defun dmr:comment-header-info ()
  (interactive)
  (let ((current-date (shell-command-to-string "echo -n $(date +%F)")))
    (insert (format "(%s, %s): " (user-login-name) current-date))))

(defun dmr:insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%F)")))
