;;; ../dotfiles/configs/emacs/.doom.d/lang/+latex.el -*- lexical-binding: t; -*-

;; Typst
(use-package! typst-ts-mode
  :defer t
  :custom (progn
  (typst-ts-mode-watch-options "--open")
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

:config
  (load! "+functions")
  (load! "+keybindings")


;; Add typst to list
(add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))


  ;; Necessary or else localleader is not detected
  (add-hook 'typst-ts-mode-hook #'evil-normalize-keymaps))

;; (after! typst-ts-mode
;;   (load! "+functions")
;;   (load! "+keybindings")
;;   ;; Necessary or else localleader is not detected
;;   (add-hook 'typst-ts-mode-hook #'evil-normalize-keymaps))
