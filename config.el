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

(defmacro dmr/choose-font (&rest specs)
  `(cl-find-if #'doom-font-exists-p (list ,@specs)))

;; XXX: without display-graphic-p, can't run doom-doctor
;;
;; probably because doom-font-exists-p depends on find-font, which uses the
;; frame object and thus probably depends on the window system being running.
(when (display-graphic-p)
  (setq doom-font
        (dmr/choose-font
         (font-spec :foundry "GNU" :family "Unifont" :size 16)
         ;; NOTE: hard to distinguish {} from () or []
         (font-spec :foundry "Xos4" :family "Terminus" :size 16)
         (font-spec :foundry "Misc" :family "Misc Ohsnap" :size 14)
         (font-spec :foundry "EFont" :family "Fixed" :size 16)
         (font-spec :foundry "Misc" :family "Misc Tamsyn" :size 20)
         (font-spec :foundry "Misc" :family "Misc Tamsyn" :size 16)
         (font-spec :family "Gohu GohuFont" :size 14)
         (font-spec :family "Dina" :size 13)
         "Gohu GohuFont:pixelsize=14"
         (font-spec :family "gohufont" :size 14))))

;; (setq bad-fonts `(,(font-spec :foundry "Type Design" :family "Kissinger 2" :size 16)
;;                   ,(font-spec :foundry "Type Design" :family "Kissinger 2f" :size 16)
;;                   ,(font-spec :family "ProFont" :size 16)
;;                   ,(font-spec :family "ProggyCleanTT" :size 18)
;;                   ,(font-spec :family "ProggyCleanCP.pcf" :size 11)))


;; Keep us from changing text size since it looks awful
(after! evil
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

;;;; END-FONTY-STUFF ;;;;

;;;; free more keys ;;;;

(keymap-global-unset "C-\\")

;; other free keys ;;
;;
;; C-=
;; M-o
;; M-[
;; M-]
;; M-+
;; M-#
;; M-*
;; S-M-o
;; C-: (C-S-;)
;; C-" (C-S-')
;; C-| (C-S-\)
;;
;; moar freeable(?) keys
;;
;; global-map:
;;
;; M-i   -> tab-to-tab-stop
;; M-j   -> default-indent-new-line
;; C-M-j -> default-indent-new-line
;; M-k   -> kill-sentence
;; M-t   -> transpose-word
;; M-e   -> forward-sentence
;; M-a   -> backward-sentence
;; M-c   -> capitalize-word
;; M-z   -> zap-to-char
;; M-=   -> count-words-region
;; M-~   -> not-modified
;; M-@   -> mark-word
;; M-$   -> ispell-word
;; M-(   -> insert-parentheses
;; M-\   -> delete-horizontal-space
;;
;; global-map (but lolwut):
;;
;; M-`   -> tmm-menubar

;;;; theme/visuals ;;;;

;; NOTE: wanted to do some fine-tuning, like below, but this messed up colors in
;; terminal somehow
;;
;; (custom-theme-set-faces! 'doom-one
;;   '(default
;;      ;:background "#01242b"
;;      :background "#01141b"
;;      :foreground "#dbd2df"))
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; (setq display-line-numbers-type nil)

;; for xterms; means "not-flashing cursor"
(setq visible-cursor nil)

;; This turns on sometimes
(add-hook 'messages-buffer-mode-hook #'doom-disable-line-numbers-h)

;; XXX: i'm not sure i defined the fn correctly in autoloads
;;
(global-set-key [remap doom/delete-frame-with-prompt]
                #'dmr/delete-frame-with-prompt-if-last)
(global-set-key [remap delete-frame]
                #'dmr/delete-frame-with-prompt-if-last)

;; Otherwise we always hit hl-line-face
(define-advice face-at-point (:around (&rest args))
  (if hl-line-mode
      (unwind-protect
          (progn (hl-line-mode -1)
                 (apply args))
        (hl-line-mode +1))
    (apply args)))

;;;; ace ;;;;

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

;;;; mouse ;;;;

(use-package! inhibit-mouse
  :config (inhibit-mouse-mode))

;; NOTE: I think this stuff is useless
(defun dmr:disable-mouse ()
  (setq track-mouse nil))

(add-hook! 'window-configuration-change-hook #'dmr:disable-mouse)

(setq track-mouse nil)

;;;; env ;;;;

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;;;; evil ;;;;

(defun dmr:test-rebind ()
  (interactive)
  (message "LZOLZLZZZZZZ"))


;; NOTE: keeping this as an example; its actually solved via early
;; evil-disable-insert-state-bindings.
;;
;; (after! evil
;;   (map! :map evil-insert-state-map
;;         (:prefix "C-x"
;;          :i "C-s" #'save-buffer)))

(after! evil
  (map! :map evil-insert-state-map
        "M-n" #'evil-next-line
        "M-p" #'evil-previous-line))


  ;; NOTE: cant do this bc it actually is RET
  ;;
  ;; (map! :map evil-motion-state-map
  ;;       "C-m" #'dmr:test-rebind))

;;;; Parens ;;;;

;; dumb parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;;; Projectile ;;;;

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

;;;; Ivy ;;;;
;;
;; NOTE: fun fact: we don't use Ivy anymore (moved to corfu/vertico). Should
;; probably remove this soon, but keeping it for ref for the moment.

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

;;;; doom/reload ;;;;

(defun dmr:handle-compile-buffer (_buf status)
  (when (equal status "finished\n")
    (kill-buffer _buf)))

(setq doom-handle-compile-buffer-fn #'dmr:handle-compile-buffer)

;;;; package management ;;;;

;; NOTE: dmr:package-get-url depends on a fn in straight, so we load it.
(use-package! straight)

(defun dmr:pkg-url ()
  (interactive)
  (if-let ((pkg (symbol-at-point))
           (url (dmr:package-get-url pkg)))
      (progn
        (kill-new url)
        (message url))
    (user-error "no pkg")))

(defun dmr:package-get-url (pkg)
  (let ((recipe (doom-package-recipe pkg)))
    (let ((host (plist-get recipe :host))
          (repo (plist-get recipe :repo)))
      (if (not (or host repo))
          (user-error "idk")
        (pcase host
          ('github (concat "github.com/" repo))
          ('codeberg (concat "codeberg.org/" repo))
          (_ (user-error "idk host %s" host)))))))

;;;; LSP ;;;;

(defvar +lsp-auto-start-modes nil)

(define-advice lsp! (:around (&rest args))
  ;; NOTE: lsp! isn't actually supposed to be called interactively, but gonna
  ;; leave that exception in here for now
  (when (or (called-interactively-p 'interactive)
            (apply 'derived-mode-p +lsp-auto-start-modes))
    (apply args)))

;; read ~/.emacs.d/lisp/doom.el for a list of hooks and their order of execution
(remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)

;;;; version control ;;;;

(map! :leader
      (:prefix-map ("g" . "git")
       (:when (modulep! :tools magit)
        :desc "Magit refresh" "z" #'magit-refresh)))

(defun dmr:cmd->str (cmd)
  (string-trim-right
   (shell-command-to-string cmd)))

(defun dmr:yank-fn (meth &rest args)
  (lambda ()
    (interactive)
    (kill-new
     (apply meth args))))

(defconst dmr:git-full-commit-cmd "git rev-parse HEAD")
(defconst dmr:git-short-commit-cmd "git rev-parse --short HEAD")
(fset 'dmr:yank-head-commit (dmr:yank-fn 'dmr:cmd->str dmr:git-full-commit-cmd))
(fset 'dmr:yank-short-commit (dmr:yank-fn 'dmr:cmd->str dmr:git-short-commit-cmd))

(map! :leader
      (:prefix-map ("g" . "git")
                   (:prefix-map ("i" . "info")
                                ;; XXX: do we want this condition?
                                (:when (modulep! :tools magit)
                                  :desc "Yank head short commit" "s" #'dmr:yank-short-commit
                                  :desc "Yank head commit" "y" #'dmr:yank-head-commit))))
;;;; file templates ;;;;

;; NOTE: this allows us to load the file templates code and use it manually.
(setq +file-templates-inhibit t)

(after! yasnippet
  (remove-hook 'doom-switch-buffer-hook #'+file-templates-check-h)
  (remove-hook 'doom-escape-hook #'yas-abort-snippet))

;;;; browser ;;;;

(setq browse-url-browser-function 'eww-browse-url)

;; globally disable eww images with:
;;
;; (setq shr-inhibit-images t)

;;;; corfu ;;;;

(defun dmr:corfu-complete-idx (k)
  (let ((corfu--index k))
    (corfu--insert 'finished)))

(defmacro dmr:corfu-make-complete-idx (k)
  `(lambda ()
     (interactive)
     (dmr:corfu-complete-idx ,k)))

;; XXX TODO: check for corfu too?
(map! :when (modulep! :editor evil +everywhere)
      :map corfu-map
      "M-7" (dmr:corfu-make-complete-idx 2)
      "M-8" (dmr:corfu-make-complete-idx 3)
      "M-9" (dmr:corfu-make-complete-idx 4)
      "M-0" (dmr:corfu-make-complete-idx 5)
      "C-\\" #'+corfu/move-to-minibuffer)

;;;; comments ;;;;

(after! newcomment
  (setq comment-empty-lines t))

(map! :g "M-;" 'comment-line)

;;;; eldoc ;;;;

(add-hook! 'eldoc-mode-hook
  (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area 'local))
;; (after! eldoc
;;   (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area))


;;;; misc utils ;;;;

(defun dmr:comment-header-info ()
  (interactive)
  (let ((current-date (shell-command-to-string "echo -n $(date +%F)")))
    (insert (format "(%s, %s): " (user-login-name) current-date))))

(defun dmr:insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%F)")))

;;;; wtf even is this? ;;;;

;;; which-key expt
;;(setq which-key-idle-delay 10000)
(setq which-key-idle-delay 1.0)
(setq which-key-idle-secondary-delay 0.05)

;; (setq load-file-rep-suffixes '("" ".gz"))
;; (auto-compression-mode 1)

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

;; NOTE: doom sets this to fundamental-mode for some reason
(setq initial-major-mode 'lisp-interaction-mode)

(setq source-directory
      (cl-find-if #'file-exists-p
                  '("~/git/tool/editor/repos-emacs/emax"
                    "~/git/tool/editor/repos-emacs/full"
                    "~/git/tool/editor/repos-emacs/emacs")))

;; Doom-specific
(defun undo-pin-truncation ()
  (let ((keyword '("(package!\\_>" (0 (+emacs-lisp-truncate-pin)))))
    (font-lock-remove-keywords nil (list keyword))))
(add-hook! 'emacs-lisp-mode-hook #'undo-pin-truncation)
(add-hook! 'elisp-mode-hook #'undo-pin-truncation)

;;;; Common Lisp ;;;;

(setq +lisp-quicklisp-paths '("~/.config/quicklisp"))

(defun dmr:maybe-add-scheme (s)
  (when s
    (concat "file://" s)))

(setq common-lisp-hyperspec-root
      (or (dmr:maybe-add-scheme
           (cl-find-if #'file-exists-p
                       '("/fake/omg"
                         "/usr/share/doc/clhs/HyperSpec/")))
          "http://www.lispworks.com/reference/HyperSpec/"))

;;;; Scheme ;;;;

(setq geiser-chez-binary "chez")

;;;; Clojure ;;;;

(after! clj-refactor
  (setq cljr-add-ns-to-blank-clj-files nil))

;;;; json ;;;;

(after! json-mode
  (setq json-reformat:indent-width 2))

;;;; js ;;;;

(after! js
  (setq js-indent-level 2))

(after! typescript-mode
  (setq typescript-indent-level 2))

;;;; Org ;;;;

(setq org-directory "~/code/elisp/org-dir/")

(after! org
  (setq org-link-descriptive nil)
  (setq org-startup-indented nil)
  (setq org-list-demote-modify-bullet nil)
  (setq org-hide-leading-stars nil))

;; NOTE: I _think_ this is a deprecated variable
;;
;; (after! org
;;   (setq org-export-allow-bind-keywords t))

;;;; Markdown ;;;;

(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :i "M-b" #'backward-word))

;;;; Python ;;;;

(map! :localleader
      :map python-mode-map
      :desc "LSP on" "l" #'lsp)

;; TODO: want to be able to disable lsp, but nontrivial. There's 2 steps:
;; 0. set the buffer modes to non-lsp
;; 1. turn off the lsp server

;; XXX: this is the default. I don't think it matters.
(setq lsp-keep-workspace-alive nil)

;; Should work okay
(defun dmr:hard-kill-py-lsp ()
  (let ((store lsp-restart))
    (setq lsp-restart 'ignore)
    (kill-buffer "*pylsp*")
    (setq lsp-restart store)))


;; XXX: this is flaky & I can't tell why.
(defun dmr:try-shutdown-lsp ()
  (interactive)
  (let ((ws (lsp-workspaces)))
    (if (eq (length ws) 1)
        (let ((w (car ws)))
          (with-lsp-workspace
              (lsp--shutdown-workspace))))
    (message "shutdown fail (%s)" (length ws))))

(defun dmr:turn-off-py-lsp ()
  (interactive)
  (dmr:try-shutdown-lsp)
  (dmr:hard-kill-py-lsp))

;;;; AsciiDoc ;;;;

(after! adoc-mode
  ;; TODO: better font config
  (custom-set-faces
   ;; '(adoc-complex-replacement-face
   ;;   ((default (:inherit adoc-meta-face))))

   '(adoc-meta-face ((default (:family nil)))))

  (set-face-attribute 'adoc-complex-replacement-face nil :box nil)
  ;; (set-face-attribute 'adoc-meta-face nil :family nil)
  (setq adoc-script-raise '(0.0 0.0)))

;;;; pkgbuild-mode ;;;;

(after! pkgbuild-mode
  (setq pkgbuild-update-sums-on-save nil)
  (setq pkgbuild-template "")
  (setq pkgbuild-initialize nil))

;;;; email ;;;;

(add-to-list 'auto-mode-alist
             '("/tmp/neomutt-" . mail-mode))

(defun dmr:mail-mode-hook ()
  (setq fill-column 60))

(add-hook! mail-mode
           #'dmr:mail-mode-hook
           #'doom-disable-line-numbers-h)

;;;; end? ;;;;

;;;; ^ just dev ^ ;;;;
