;;; ../dotfiles/configs/emacs/.doom.d/tools/+pdf.el -*- lexical-binding: t; -*-
(with-eval-after-load 'pdf-tools
  (load! "+functions")
  (load! "+keybindings")


  (defun sbraun/enable-pdf-view-midnight-minor-mode ()
    (pdf-view-midnight-minor-mode))

  ;; pdf-tools midnight colors
  ;; (use-package! pdf-tools
  ;;   :defer
  ;;   :config
  ;;   ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))  ;; black-white
  ;;   ;; (setq pdf-view-midnight-colors '("#bbc2cf" . "#282c34" ))  ;; doom-one
  ;;   )

  )
