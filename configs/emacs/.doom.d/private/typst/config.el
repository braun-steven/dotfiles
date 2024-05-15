;;; ../dotfiles/configs/emacs/.doom.d/lang/+latex.el -*- lexical-binding: t; -*-
(after! typst-ts-mode
  (load! "+functions")
  (load! "+keybindings")
  ;; Necessary or else localleader is not detected
  (add-hook 'typst-ts-mode-hook #'evil-normalize-keymaps))
