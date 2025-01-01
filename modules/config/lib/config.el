;;; config/lib/config.el -*- lexical-binding: t; -*-

;; yanking ;;

(defun dmr:cmd->str (cmd)
  (string-trim-right
   (shell-command-to-string cmd)))

(defun dmr:yank-fn (meth &rest args)
  (lambda ()
    (interactive)
    (kill-new
     (apply meth args))))

;; package management ;;
(defun dmr:pkg-url ()
  (interactive)
  (if-let ((pkg (symbol-at-point))
           (url (package-get-url pkg)))
      (progn
        (kill-new url)
        (message url))
    (user-error "no pkg")))

(defun package-get-url (pkg)
  (let ((recipe (doom-package-recipe pkg)))
    (let ((host (plist-get recipe :host))
          (repo (plist-get recipe :repo)))
      (if (not (or host repo))
          (user-error "idk")
        (pcase host
          ('github (concat "github.com/" repo))
          (_ (user-error "idk host %s" host)))))))

(defun dmr/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
