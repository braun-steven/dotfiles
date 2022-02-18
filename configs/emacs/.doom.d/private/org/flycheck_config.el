;;; ../dotfiles/configs/emacs/.doom.d/lang/+org.el -*- lexical-binding: t; -*-

;; Set directioreis of org, org-roam and deft
(setq deft-directory "~/org/notes/")
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/notes")

;; Fix org capture buffer mode not working
(after! org
  (defadvice! dan/org-capture-prevent-restart (fn &rest args)
    :around #'+org--restart-mode-h
    (unless (bound-and-true-p org-capture-mode)
      (apply fn args)))
  (add-hook! 'org-capture-after-finalize-hook
             (let ((buffer (org-capture-get :buffer)))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (+org--restart-mode-h))))))


;; Org setup
(after! org
  (load! "+functions")
  (load! "+keybindings")
  (load! "+org-setup"))
