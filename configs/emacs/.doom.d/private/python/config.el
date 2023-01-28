;;; ../dotfiles/configs/emacs/.doom.d/lang/+python.el -*- lexical-binding: t; -*-
(after! python
  (load! "+functions")
  (load! "+keybindings")

  ;; Doom modeline
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'

  ;; Set flycheck config and executable
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-python-flake8-executable "flake8")

  ;; Make word motions ignore snake case underscores
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'python-mode-hook #'(lambda () (setq fill-column 100)))

  (setq company-minimum-prefix-length 2)

  (setq pyimport-pyflakes-path "~/.local/bin/pyflakes")

  ;; Set correct conda variables
  (use-package! conda
    :after python
    :config
    (setq conda-env-home-directory (expand-file-name "~/.conda"))
    (setq conda-anaconda-home (expand-file-name "~/.conda")))

  ;; Python docstring tool
  (use-package! py-pyment
    :after python
    :config
    (setq py-pyment-options '("--output=google")))

  ;; Fix for confusing yasnippet results in completion
  ;; (setq! +lsp-company-backends
  ;;        (if (modulep! :editor snippets)
  ;;            '(:separate company-yasnippet company-capf)
  ;;          'company-capf))

;; accept completion from copilot and fallback to company
;; (use-package! copilot
;; :hook (prog-mode . copilot-mode)
;; :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;         ("C-<tab>" . 'copilot-accept-completion-by-word)
;;         :map copilot-completion-map
;;         ("<tab>" . 'copilot-accept-completion)
;;         ("TAB" . 'copilot-accept-completion)))
  )
