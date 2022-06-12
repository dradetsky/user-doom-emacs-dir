;;; emacs/vc-lite/config.el -*- lexical-binding: t; -*-
;;
;; lightweight version of default emacs/vc module

(use-package! git-commit
  ;; XXX: dunno what this is
  ;;
  ;; :hook (doom-first-file . global-git-commit-mode)
  :config
  (set-yas-minor-mode! 'git-commit-mode)

  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (setq-hook! 'git-commit-mode-hook fill-column 72)

  (add-hook! 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state)))))
