;;; ../dotfiles/configs/emacs/.doom.d/tools/+lsp.el -*- lexical-binding: t; -*-
(after! lsp-mode
  (load! "+functions")
  (load! "+keybindings")


  ;; Ignore __pycache__ directories
  (push "[/\\\\]_pycache_\\'" lsp-file-watch-ignored-directories)

  ;; Enable lsp-header-breadcrumbs (similar to which-func mode but lsp wide)
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Set file-watcher higher
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-enable-folding t)

  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-ui-doc-delay 0.5)
  ;; (setq lsp-ui-doc-position 'at-point)


  ;; (use-package! lsp-grammarly
  ;;       :init (load! "keytar")
  ;;       :hook (markdown-mode . (lambda ()
  ;;               (require 'lsp-grammarly)
  ;;               (lsp))))  ; or lsp-deferred

  ;; From https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
  ;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  )
