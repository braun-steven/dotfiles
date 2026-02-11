;;; ../dotfiles/configs/emacs/.doom.d/lang/+python.el -*- lexical-binding: t; -*-
(with-eval-after-load 'python
  (load! "+functions")
  (load! "+keybindings")

  ;; Doom modeline
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'

  ;; Set flycheck config and executable
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-python-flake8-executable "flake8")

  ;; Make word motions ignore snake case underscores
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; (add-hook 'python-mode-hook #'(lambda () (setq fill-column 100)))

  ;; (setq pyimport-pyflakes-path "~/.local/bin/pyflakes")


  (with-eval-after-load 'lsp-pyright
    (setq lsp-pyright-auto-import-completions nil))

  (add-hook 'python-mode-hook (lambda () (setq-local corfu-auto-prefix 2)))

  ;; Set correct conda variables
  ;; (use-package! conda
  ;;   :after python
  ;;   :config
  ;;   (setq conda-env-home-directory (expand-file-name "~/.conda"))
  ;;   (setq conda-anaconda-home (expand-file-name "~/.conda")))

  )
