;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
                                        ;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
                                        ;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))


(package! yasnippet-snippets)
(package! lorem-ipsum)
(package! ssh-agency)
(package! dockerfile-mode)
(package! yaml-mode)

;; Julia LSP
;; (package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))

;; Collection of Ridiculously Useful eXtensions for Emacs
(package! crux)

(package! git-link)
;; (package! git-link :pin "40f7b1674d2c703199ff2f82b464f17aa6f61b4b")  ;; Fixes transient issue

(package! typst-ts-mode :recipe (:host codeberg :repo "meow_king/typst-ts-mode"))

(package! benchmark-init)

(package! magit-todos)

;; (package! gptel)
(package! gptel :recipe (:nonrecursive t))
(package! gptel-prompts :recipe (:host github :repo "jwiegley/gptel-prompts"))
(package! copilot-chat) ;; Usefull for diff-based git-commit messages


(package! powerthesaurus)

;; Fish mode
(package! fish-mode)

(package! jinx)

(package! websocket)

(package! titlecase)

;; Load private module packages
(dolist (file (directory-files "~/.doom.d/private/" t directory-files-no-dot-files-regexp))
  (load! (concat file "/packages.el")))


