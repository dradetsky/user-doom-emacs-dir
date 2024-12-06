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
;; C-q   -> quoted-insert
;; C-e   -> evil-scroll-line-down
;; C-y   -> evil-scroll-line-up
;;
;; global-map (but lolwut):
;;
;; M-`   -> tmm-menubar
;;
;; free evil keys:
;; g h
;; g [
;; g ]
;; g '
;;
;; freeable(?) evil keys
;;
;; s -> evil-snipe-s
;; f -> evil-snipe-f
;; t -> evil-snipe-t
;;
;; less-good free keys
;;
;; C-w C-/

;;;; theme/visuals ;;;;

;; NOTE: wanted to do some fine-tuning, like below, but this messed up colors in
;; terminal somehow
;;
;; (custom-theme-set-faces! 'doom-one
;;   '(default
;;      ;:background "#01242b"
;;      :background "#01141b"
;;      :foreground "#dbd2df"))

;; NOTE: without this, background in terminal is blue
(custom-theme-set-faces! 'doom-one
  '(default
     :background "#000000"))

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

;;;; modeline ;;;;

(after! doom-modeline
  ;; NOTE: show misc-info on LHS
  (doom-modeline-def-modeline 'dmr/main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info misc-info)
    '(compilation objed-state persp-name battery grip irc mu4e gnus github
      debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time))

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'dmr/main 'default))))

(use-package! hl-line)

(defface hl-line-alert
  '((t :inherit hl-line :background "firebrick"))
  "Dox"
  :group 'hl-line)

(defun dmr/alert/add-ml-msg (msg)
  (add-to-list 'global-mode-string msg)
  (force-mode-line-update))

(defun dmr/alert/del-ml-msg (msg)
  (setq global-mode-string
        (delete msg global-mode-string))
  (force-mode-line-update))

(defun dmr/alert/reset-hl-line (face buf)
  (with-current-buffer buf
    (setq hl-line-face face)
    (hl-line-mode -1)
    (hl-line-mode +1)))

(defun dmr/alert/begin (msg buf)
  (progn
    (dmr/alert/add-ml-msg msg)
    (dmr/alert/reset-hl-line 'hl-line-alert buf)))

(defun dmr/alert/end (msg buf)
  (progn
    (dmr/alert/del-ml-msg msg)
    (dmr/alert/reset-hl-line 'hl-line buf)))

(defvar alert/init-buffer nil)

(defmacro with-cmd-alert (msg)
  (let ((text (propertize msg 'face '(doom-modeline-urgent bold))))
    `(lambda (&rest args)
       (interactive
        (lambda (spec)
          (progn
            (setq alert/init-buffer (current-buffer))
            (dmr/alert/begin ,text alert/init-buffer)
            (advice-eval-interactive-spec spec))))
       (unwind-protect
           (apply args)
         (dmr/alert/end ,text alert/init-buffer)))))

(defun dmr/modeline-vcs-name ()
  (and vc-mode
       (if-let ((idx (string-match "[:-]+" vc-mode)))
           (substring vc-mode (match-end 0))
         (vc-mode))))

(after! doom-modeline
  (setq doom-modeline-vcs-max-length 16))

(use-package! avy)

(defun dmr/avy-goto-char-region (char beg end)
  (avy-with avy-goto-char
    (avy-jump
     (regexp-quote (string char))
     :beg beg
     :end end)))

(defun dmr/avy-goto-char-fwd (char)
  (interactive
   (list (read-char "char: " t)))
  (dmr/avy-goto-char-region char
                            (point)
                            (window-end)))

(defun dmr/avy-goto-char-rev (char)
  (interactive
   (list (read-char "char: " t)))
  (dmr/avy-goto-char-region char
                            (window-start)
                            (point)))

(advice-add 'avy-goto-char :around (with-cmd-alert "====> AVY-GOTO-CHAR"))
(advice-add 'dmr/avy-goto-char-rev :around (with-cmd-alert "====> AVY-GOTO-CHAR-REV"))
(advice-add 'dmr/avy-goto-char-fwd :around (with-cmd-alert "====> AVY-GOTO-CHAR-FWD"))

;;;; avy ;;;;

;; (defconst ak-def '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(after! avy
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?i ?u)))

(map! :map evil-motion-state-map
      "gh" #'avy-goto-char
      "g[" #'dmr/avy-goto-char-rev
      "g]" #'dmr/avy-goto-char-fwd)

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

(advice-add 'ace-window :around (with-cmd-alert "====> ACE-WINDOW"))

(map! (:map evil-window-map
            "M-w" #'ace-swap-window))

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

;; XXX: Henrik advises that an update will break this unless it's changed to the
;; following:
;;
;; (defconst vc-autoload-path (doom-module-expand-path '(t :emacs vc) "autoload/vc.el"))
(defconst vc-autoload-path (doom-module-expand-path '(:emacs . vc) "autoload/vc.el"))

(autoload '+vc--remote-homepage vc-autoload-path)

(after! magit
  (setq magit-status-show-hashes-in-headers t))

(map! :leader
      (:prefix "g"
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

;; XXX: this is easier than figuring out how magit does this
(defun dmr/git-pull-no-commit ()
  (interactive)
  (shell-commmand "git pull --no-commit --ff-only"))

(defun dmr/yank-repo-ssh-url ()
  (interactive)
  ;; https://github.com/seagle0128/doom-modeline
  (if-let ((hu (+vc--remote-homepage)))
      ;; 8 = (length "https://")
      (let ((part (substring hu 8)))
        (aset part 10 (string-to-char ":"))
        (let ((url (concat "git@" part ".git")))
          (kill-new url)
          (message "Copied to clipboard: %S" url)))))

(map! :leader
      (:prefix "g"
               (:prefix "i"
                        ;; XXX: do we want this condition?
                        (:when (modulep! :tools magit)
                          :desc "Yank repo ssh url" "r" #'dmr/yank-repo-ssh-url
                          :desc "Yank head short commit" "s" #'dmr:yank-short-commit
                          :desc "Yank head commit" "y" #'dmr:yank-head-commit))))
;;;; file templates ;;;;

;; NOTE: this allows us to load the file templates code and use it manually.
(setq +file-templates-inhibit t)

(after! yasnippet
  (remove-hook 'doom-switch-buffer-hook #'+file-templates-check-h)
  (remove-hook 'doom-escape-hook #'yas-abort-snippet))

;;;; browser ;;;;

;; NOTE: still want to use this sometimes, but also want firefox set up
;;
;; (setq browse-url-browser-function 'eww-browse-url)

(defvar dmr/firefox-default-profile "privacy")
(setq browse-url-firefox-arguments (list "-P" dmr/firefox-default-profile))
(setq browse-url-firefox-new-window-is-tab t)
(setq browse-url-browser-function 'browse-url-firefox)

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

(defun dmr/comment (arg)
  (interactive "*P")
  (if (use-region-p)
      (comment-dwim arg)
    (comment-line arg)))

(map! :g "M-;" 'dmr/comment)

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

;;;; files generally ;;;;

(defun dmr/chmod-755-file ()
  (interactive)
  (let ((cmd (format "chmod 755 %s" (buffer-file-name))))
    (message cmd)
    (call-process-shell-command cmd)))

(map! :leader
      (:prefix "f"
       :desc "Make file executable" "x" #'dmr/chmod-755-file))

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
      (:prefix "e"
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

(defconst dmr/draft-mail-rx
  (eval
   (let ((home (getenv "HOME")))
     `(rx ,home
          "/docs"
          (zero-or-more (or alpha
                            "-"
                            "/"))
          ".draftmail.txt"))))

(add-to-list 'auto-mode-alist
             '("/tmp/neomutt-" . mail-mode))

(add-to-list 'auto-mode-alist
             `(,dmr/draft-mail-rx . mail-mode))

(defun dmr:mail-mode-hook ()
  (setq fill-column 60))

(add-hook! mail-mode
           #'dmr:mail-mode-hook
           #'doom-disable-line-numbers-h)

;;;; vimrc ;;;;

(after! vimrc-mode
  (add-to-list 'auto-mode-alist
               '("\.tridactylrc" . vimrc-mode)))

;;;; end? ;;;;

;;;; ^ just dev ^ ;;;;
