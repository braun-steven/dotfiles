;;; ../dotfiles/configs/emacs/.doom.d/lang/+latex.el -*- lexical-binding: t; -*-

;; Typst
(use-package! typst-ts-mode
  :defer t
  :custom (progn
  (typst-ts-mode-watch-options "--open")
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t)
  )

  :config
  (load! "+functions")
  (load! "+keybindings")


;; Add typst to list
(add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))


  ;; Necessary or else localleader is not detected
  (add-hook 'typst-ts-mode-hook #'evil-normalize-keymaps))

;; (with-eval-after-load 'eglot
;;   (with-eval-after-load 'typst-ts-mode
;;     (add-to-list 'eglot-server-programs
;;                  `((typst-ts-mode) .
;;                    ,(eglot-alternatives `(,typst-ts-lsp-download-path
;;                                           "tinymist"
;;                                           "typst-lsp"))))))

(use-package! typst-ts-mode
  :custom
  (typst-ts-mode-watch-options "--open")
  :config
  ;; make sure to install typst-lsp from
  ;; https://github.com/nvarner/typst-lsp/releases
  ;; or use tinymist
  (after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "typst-lsp")
    :major-modes '(typst-ts-mode)
    :server-id 'typst-lsp))
  ))
