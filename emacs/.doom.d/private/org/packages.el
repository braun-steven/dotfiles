;; -*- no-byte-compile: t; -*-


;; (unpin! org-roam)
;; (package! org-roam-ui)

;; Temp fix for emacsql issue with org-roam, see also: https://github.com/org-roam/org-roam/issues/2485
;; (package! emacsql :pin "491105a01f58bf0b346cbc0254766c6800b229a2")


(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
