;;; ../dotfiles/configs/emacs/.doom.d/+modules/lang/python/functions.el -*- lexical-binding: t; -*-



(defun sbraun/conda-env-activate ()
  "Wrapper for pyvenv-activate that also restarts the lsp-sessions."
  (interactive)
  (call-interactively 'conda-env-activate)
  (call-interactively 'lsp))  ;; lsp-mode
  ;; (call-interactively 'eglot-reconnect))          ;; eglot-mode

(defun sbraun/pdb-insert ()
  "Insert pdb statement."
  (interactive)
  (evil-open-above 1)
  (insert "breakpoint()")
  (evil-normal-state)
  (evil-next-line))
