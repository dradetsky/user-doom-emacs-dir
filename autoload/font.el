;;;###autoload
(defun doom-font-exists-p (font)
  "Return non-nil if FONT exists on this system."
  (declare (pure t) (side-effect-free t))
  (ignore-errors (find-font (doom-normalize-font font))))

(defun doom-font-exists-with-warning-p (font)
  (let ((foundp (doom-font-exists-p font)))
    (unless foundp
      (message (format "couldnt find font %s" font)))
    foundp))
