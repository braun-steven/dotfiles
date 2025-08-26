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


(defun sbraun/insert-bibtex-key-at-point ()
  "Insert a BibTeX key at point by searching keys and titles from `bibtex-lookup-bibfile`."
  (interactive)
  (unless bibtex-lookup-bibfile
    (user-error "Variable `bibtex-lookup-bibfile` is not set"))
  (let* ((python-script "bibkeys") ;; update this path
         (bibfile (expand-file-name bibtex-lookup-bibfile (projectile-project-root)))
         (python-cmd (format "%s %s"
                             (shell-quote-argument python-script)
                             (shell-quote-argument bibfile)))
         (output (shell-command-to-string python-cmd))
         (lines (split-string output "\n" t))
         ;; Parse into (key . title)
         (entries
          (mapcar (lambda (line)
                    (let ((parts (split-string line " | ")))
                      (cons (car parts) (cadr parts))))
                  lines))
         ;; Calculate max key width for padding
         (max-key-len (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
         ;; Prepare candidates with padded key and title separated by spaces only
         (candidates
          (mapcar (lambda (entry)
                    (let ((key (car entry))
                          (title (cdr entry)))
                      (cons (format (format "%%-%ds  %%s" max-key-len) key title) key)))
                  entries))
         (chosen (completing-read "Select BibTeX entry: " candidates nil t)))
    ;; Insert only the key part (cdr of chosen)
    (insert (format "@%s" (cdr (assoc chosen candidates))))))
