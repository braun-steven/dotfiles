;;; ../dotfiles/configs/emacs/.doom.d/+modules/lang/org/functions.el -*- lexical-binding: t; -*-

(defun sbraun/org-emphasize-dwim (char)
  "DWIM (Do What I Mean) wrapper for org-emphasize.
   If there's an active region, apply emphasis to it.
   Otherwise, apply emphasis to the word at point.
   CHAR is the emphasis character to use."
  (interactive)
  ;; Check if there is an active region (e.g., text is selected).
  (if (use-region-p)
      ;; If a region is active, apply emphasis to the selected region.
      (org-emphasize char)
    ;; Otherwise, apply emphasis to the word at point.
    (save-excursion
      ;; Find the boundaries of the word at point.
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (goto-char (car bounds))
          (set-mark (cdr bounds))
          ;; Apply emphasis to the selected word.
          (org-emphasize char)
          (deactivate-mark))))))
