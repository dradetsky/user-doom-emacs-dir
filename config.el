;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "dmr"
      user-mail-address "dmr@dmr.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONT FIX
;;
;; sudo fc-cache --force
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; broken by switch to native
;;
;;(setq doom-font (font-spec :family "gohufont" :size 14))
;;
;; too small
;;
;;(setq doom-font "Cozette:style=Medium")
;;
;; bold caps unreadable
;;
;;(setq doom-font "Spleen:style=Regular")

(setq doom-font
      (cl-find-if #'doom-font-exists-p
                  '("Gohu GohuFont:pixelsize=14"
                    (font-spec :family "Gohu GohuFont" :size 14)
                    (font-spec :family "gohufont" :size 14))))

;; (setq doom-font
;;       (font-spec :family "Gohu GohuFont" :size 14))
;;       (font-spec :family "gohufont" :size 14))
;;       "Gohu GohuFont:pixelsize=14")

;; NOTE: wanted to do some fine-tuning, like below, but this messed up colors in
;; terminal somehow
;;
;; (custom-theme-set-faces! 'doom-one
;;   '(default
;;      ;:background "#01242b"
;;      :background "#01141b"
;;      :foreground "#dbd2df"))
(setq doom-theme 'doom-one)

(setq org-directory "~/code/elisp/org-dir/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; (setq display-line-numbers-type nil)

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

;; for xterms; means "not-flashing cursor"
(setq visible-cursor nil)

(after! newcomment
  (setq comment-empty-lines t))

(after! org
  (setq org-startup-indented nil)
  (setq org-list-demote-modify-bullet nil)
  (setq org-hide-leading-stars nil))

(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :i "M-b" #'backward-word))

(add-hook! 'eldoc-mode-hook
  (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area 'local))
;; (after! eldoc
;;   (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area))

;;; which-key expt
;;(setq which-key-idle-delay 10000)
(setq which-key-idle-delay 1.0)
(setq which-key-idle-secondary-delay 0.05)

;; (setq load-file-rep-suffixes '("" ".gz"))
;; (auto-compression-mode 1)

(defun dmr:comment-header-info ()
  (interactive)
  (let ((current-date (shell-command-to-string "echo -n $(date +%F)")))
    (insert (format "(%s, %s): " (user-login-name) current-date))))

(defun dmr:insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%F)")))

(defun dmr:yank-proj-rel-buffer-filename ()
  (interactive)
  (if (projectile-project-root)
      (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
          (let ((relname (substring filename (length (projectile-project-root)))))
            (message (kill-new relname)))
        (error "Couldn't find filename in current buffer"))
    (error "Not in project")))

(map! :leader
      (:prefix ("p" . "project")
       :desc "toggle project r/o" "E" #'projectile-toggle-project-read-only
       :desc "yank project file name" "y" #'dmr:yank-proj-rel-buffer-filename))

(defun dmr:string-length< (str0 str1)
  (let ((len0 (length str0))
        (len1 (length str1)))
    (< len0 len1)))

(after! ivy
  (add-to-list 'ivy-sort-functions-alist
               '(+ivy/switch-to-buffer . dmr:string-length<))
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-projectile-switch-project . dmr:string-length<))
  ;; this one doesn't appear to work for SPC-f-f
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-find-file . dmr:string-length<))
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-projectile-find-file . dmr:string-length<)))

(after! evil
  ;; mouse-wheel-text-scale
  (global-unset-key [C-mouse-4])
  (global-unset-key [C-mouse-5])
  ;; mouse-wheel-global-text-scale
  (global-unset-key [C-M-mouse-4])
  (global-unset-key [C-M-mouse-5])
  ;; mouse-wheel-global-text-scale
  (keymap-global-unset "C-<wheel-down>")
  (keymap-global-unset "C-<wheel-up>")
  (keymap-global-unset "C-M-<wheel-down>")
  (keymap-global-unset "C-M-<wheel-up>")
  ;; text-scale-pinch
  (keymap-global-unset "<pinch>")
  ;; text-scale-adjust
  (keymap-global-unset "C-x C-0")
  (keymap-global-unset "C-x C-=")
  (keymap-global-unset "C-x C--")
  (keymap-global-unset "C-x C-+")
  ;; global-text-scale-adjust
  (keymap-global-unset "C-x C-M-0")
  (keymap-global-unset "C-x C-M-=")
  (keymap-global-unset "C-x C-M--")
  (keymap-global-unset "C-x C-M-+")
  (undefine-key! evil-normal-state-map
    ;; doom/increase-font-size
    "M-C-="
    ;; doom/decrease-font-size
    "M-C--"
    ;; #'text-scale-increase
    "C-="
    ;; #'text-scale-decrease
    "C--"))

(defun undo-pin-truncation ()
  (let ((keyword '("(package!\\_>" (0 (+emacs-lisp-truncate-pin)))))
    (font-lock-remove-keywords nil (list keyword))))
(add-hook! 'emacs-lisp-mode-hook #'undo-pin-truncation)
(add-hook! 'elisp-mode-hook #'undo-pin-truncation)

;; XXX: i'm not sure i defined the fn correctly in autoloads
;;
(global-set-key [remap doom/delete-frame-with-prompt]
                #'dmr/delete-frame-with-prompt-if-last)
(global-set-key [remap delete-frame]
                #'dmr/delete-frame-with-prompt-if-last)

(map! :g "M-;" 'comment-line)

;; free more keys ;;

(keymap-global-unset "C-\\")

;; other free keys ;;
;;
;; C-|

(after! disable-mouse
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

;;;; Modeline ;;;;
;;
;; NOTE: install ttf-nerd-fonts-symbols-mono


;;;; indent/tab ;;;;

;; indent-for-tab-command
;;
;; tab-always-indent
;; tab-first-completion
;;
;; indent-line-function

(setq tab-always-indent 'complete)
;; (setq tab-first-completion nil)

;;;; LSP ;;;;

(defvar +lsp-auto-start-modes '())

(define-advice lsp! (:around (&rest args))
  ;; NOTE: lsp! isn't actually supposed to be called interactively, but gonna
  ;; leave that exception in here for now
  (when (or (called-interactively-p 'interactive)
            (apply 'derived-mode-p +lsp-auto-start-modes))
    (apply args)))

;; read ~/.emacs.d/lisp/doom.el for a list of hooks and their order of execution
(remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)

;;;; elisp ;;;;

(defmacro with-curr-defun (act)
  `(save-excursion
    (beginning-of-defun)
    (end-of-defun)
    (,act)))

(defun eval-print-defun ()
  (interactive)
  (with-curr-defun (lambda () (eval-print-last-sexp))))

(map! :localleader
      :map (emacs-lisp-mode-map lisp-interaction-mode-map)
      (:prefix ("e" . "eval")
               "p" #'eval-print-defun))

;;;; Common Lisp ;;;;

(setq +lisp-quicklisp-paths '("~/.config/quicklisp"))

;;;; Clojure ;;;;

(after! clj-refactor
  (setq cljr-add-ns-to-blank-clj-files nil))
