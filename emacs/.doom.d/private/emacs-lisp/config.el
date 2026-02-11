;;; ../dotfiles/configs/emacs/.doom.d/lang/+elisp.el -*- lexical-binding: t; -*-
(with-eval-after-load 'elisp-mode
  (load! "+functions")
  (load! "+keybindings")

  ;; Use aggressive indenting in emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))
