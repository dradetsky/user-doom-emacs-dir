;;;###autoload
(defun dmr/delete-frame-with-prompt-if-last ()
  "dox"
  (interactive)
  (let ((num-clients (length server-clients)))
    (if (= 0 num-clients)
        (doom/delete-frame-with-prompt)
      (delete-frame))))
