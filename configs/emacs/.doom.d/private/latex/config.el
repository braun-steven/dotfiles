;;; ../dotfiles/configs/emacs/.doom.d/lang/+latex.el -*- lexical-binding: t; -*-
(after! tex
  (load! "+functions")
  (load! "+keybindings")

  ;; Make latex sections have a larger font
  (setq font-latex-fontify-sectioning 1.1)

  ;; Add auto-fillmode (auto line brake after 80 chars)
  ;; (add-hook 'TeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk")))

  ;; Also fontify commonly custom commands defined by \newcomand
  ;; (add-hook 'TeX-mode-hook #'(lambda () (push '("citeauthorandref" "[{") font-latex-match-reference-keywords)))

  ;; Set latex viewer
  (setq +latex-viewers '(evince))
  )
