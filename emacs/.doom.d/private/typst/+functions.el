;;; ../dotfiles/configs/emacs/.doom.d/+modules/typst/functions.el -*- lexical-binding: t; -*-


(defun sbraun/typstfmt-current-buffer ()
  "Formats the current typst document using the typstfmt binary."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "typstfmt " (shell-quote-argument (buffer-file-name))))
    (message "Buffer is not associated with a file.")))
