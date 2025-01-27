;;; ../dotfiles/configs/emacs/.doom.d/tools/+lsp.el -*- lexical-binding: t; -*-
(after! lsp-mode
  (load! "+functions")
  (load! "+keybindings")


  ;; Ignore __pycache__ directories
  (push "[/\\\\]_pycache_\\'" lsp-file-watch-ignored-directories)

  ;; Enable lsp-header-breadcrumbs (similar to which-func mode but lsp wide)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-enable t)

  ;; Set file-watcher higher
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-enable-folding t)

  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-enable t)
  ;; (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-lens-enable t)
  ;; (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-modeline-code-actions-enable t)
  ;; (setq lsp-signature-auto-activate t)
  ;; (setq lsp-signature-render-documentation t)
  ;; (setq lsp-completion-show-detail t)
  ;; (setq lsp-completion-show-kind t)

  (add-hook 'lsp-after-apply-edits-hook
            (lambda (operation)
              (when (eq operation 'rename)
                (save-some-buffers t))))


  ;; (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-ui-doc-delay 0.5)
  ;; (setq lsp-ui-doc-position 'at-point)


  (delete 'lsp-terraform lsp-client-packages)
  )
