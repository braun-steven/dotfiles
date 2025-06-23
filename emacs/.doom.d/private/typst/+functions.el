;;; ../dotfiles/configs/emacs/.doom.d/+modules/typst/functions.el -*- lexical-binding: t; -*-


(defun sbraun/typstfmt-current-buffer ()
  "Formats the current typst document using the typstfmt binary."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "typstfmt " (shell-quote-argument (buffer-file-name))))
    (message "Buffer is not associated with a file.")))

(defun find-all-labels-in-buffer ()
  "Find all labels of the form <label> in the current buffer.
Return a list of the labels as strings (without the angle brackets)."
  (interactive)
  (let ((labels '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<\\([^<>]+\\)>" nil t)
        (push (match-string 1) labels)))
    (setq labels (nreverse labels))
    (when (called-interactively-p 'interactive)
      (if labels
          (message "Found labels: %s" (string-join labels ", "))
        (message "No labels found.")))
    labels))

(defun sbraun/insert-label-from-buffer ()
  "Select and insert a <label> from the current buffer."
  (interactive)
  (let* ((labels (find-all-labels-in-buffer))
         (unique-labels (delete-dups labels)))
    (if (null unique-labels)
        (message "No labels found in buffer.")
      (let ((chosen (completing-read "Select label: " unique-labels nil t)))
        (insert (format "@%s" chosen))))))
