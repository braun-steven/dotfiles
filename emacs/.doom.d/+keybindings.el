;; Completion at point
;; (map! :nvi "C-SPC" #'company-capf)

(map! :leader
      ;; unmap first
      "SPC" nil 
      "`" nil

      "v" #'er/expand-region

      :leader
      "SPC" #'execute-extended-command

      "x"  nil
      (:prefix ("x" . "font")
       "-"  #'sbraun/decrease-font-size
       "+"  #'sbraun/increase-font-size))

;; Buffers
(map!
 ;; :leader (:prefix "b" "m" nil)
      :leader (:prefix "b" :desc "Messages" "m" #'(lambda () (interactive) (switch-to-buffer "*Messages*")))

      :leader (:prefix "b" "s" nil)
      :leader (:prefix "b" :desc "Messages" "s" #'(lambda () (interactive) (switch-to-buffer "*scratch*"))))

;; Windows
(map! :leader
      (:prefix "w"
       :desc "Maximize" "m" #'toggle-maximize-buffer
       "w"  #'ace-window
       "/" nil
       :desc "Split right"  "/" #'(lambda () (interactive)(evil-window-vsplit) (other-window 1))
       :desc "Split below"  "-" #'(lambda () (interactive)(evil-window-split) (other-window 1))))



;; Use avy for buffer wide jumps
(map!
 :nv "s" nil
 (:prefix ("s" .  "jump")
  :nv "s"   #'avy-goto-char-2
  :nv "j"   #'avy-goto-line-below
  :nv "k"   #'avy-goto-line-above
  :nv "w"   #'avy-goto-word-0-below
  :nv "b"   #'avy-goto-word-0-above
  :nv "W"   #'avy-goto-word-1-below
  :nv "B"   #'avy-goto-word-1-above
  :nv "c"   #'avy-goto-char
  :nv "t"   #'avy-goto-char-timer))

(map!
 :nv "gb" #'comment-box)

;; Helper function to realod the full config
(map! :leader
      (:prefix ("h" . "help")
       (:prefix ("r" . "reload")
        "c"  #'sbraun/reload-config)))


;; Shortcut to find dotfiles
(map! :leader
      "fD"  nil
      :desc "Dotfiles"  "fD"  #'(lambda () (interactive) (doom-project-find-file "~/dotfiles"))
      :desc "PhD Org." "np" #'(lambda () (interactive) (doom-project-find-file "~/org/notes/phd")))

(map! :leader
      (:prefix ("P" . "powerthesaurus")
       :desc "Synonyms" "s"  #'powerthesaurus-lookup-synonyms-dwim
       :desc "Antonyms" "a"  #'powerthesaurus-lookup-antonyms-dwim
       :desc "Sentence" "S"  #'powerthesaurus-lookup-sentences-dwim
       :desc "Related" "r"  #'powerthesaurus-lookup-related-dwim))


(map! :after corfu :map corfu-map :i [tab] #'corfu-next)

;; Copilot chat
(map! :leader
      (:prefix ("C" . "Copilot Chat")
       :desc "Reset" "r" #'copilot-chat-reset
       :desc "Display" "d" #'copilot-chat-display
       :desc "Explain Symbol" "e" #'copilot-chat-explain-symbol-at-line
       :desc "Explain Code" "E" #'copilot-chat-explain
       :desc "Review" "v" #'copilot-chat-review
       :desc "Document" "D" #'copilot-chat-doc
       :desc "Fix" "f" #'copilot-chat-fix
       :desc "Optimize" "o" #'copilot-chat-optimize
       :desc "Test" "t" #'copilot-chat-test
       :desc "Prompt" "p" #'copilot-chat-custom-prompt-selection
       :desc "Add Buffer" "a" #'copilot-chat-add-current-buffer
       :desc "Remove Buffer" "x" #'copilot-chat-del-current-buffer
       :desc "List Buffers" "l" #'copilot-chat-list
       :desc "Prev Prompt" "h" #'copilot-chat-prompt-history-previous
       :desc "Next Prompt" "n" #'copilot-chat-prompt-history-next
       :desc "Ask Insert" "i" #'copilot-chat-ask-and-insert
       :desc "Commit Message" "c" #'copilot-chat-insert-commit-message
       ))


;; GPTel
(map! :leader
      (:prefix ("G" . "gptel")
       :desc "gptel" "g" #'gptel
       :desc "add-buffer" "a" #'gptel-add
       :desc "add-fille" "f" #'gptel-add-file
       :desc "send" "s" #'gptel-send
       :desc "rewrite" "r" #'gptel-rewrite
       :desc "menu" "m" #'gptel-menu
       :desc "regenerate" "R" #'gptel--regenerate
       ))

(map! :leader
      (:prefix "n"
              (:prefix "r"
                       :desc "PARA" "p" #'(lambda () (interactive) (find-file "~/org/notes/20241119070534-para_hub.org")))
               ))


;; Useful, when there is a variable `bar' and I want to turn it into `foo(bar)'
(map! :after evil ;; Ensure evil and its maps are loaded
      :map evil-visual-state-map
      ;; For parentheses ()
      "s)" (kbd "S)i")  ;; Pressing 's' then ')' in visual mode will execute S, then (, then i
      "s(" (kbd "S)i")  ;; Pressing 's' then '(' in visual mode will execute S, then (, then i

      ;; For square brackets []
      "s]" (kbd "S]i")  ;; Pressing 's' then ']' in visual mode will execute S, then [, then i
      "s[" (kbd "S]i")  ;; Pressing 's' then '[' in visual mode will execute S, then [, then i

      ;; For curly braces {}
      "s}" (kbd "S}i")  ;; Pressing 's' then '}' in visual mode will execute S, then {, then i
      "s{" (kbd "S}i")) ;; Pressing 's' then '{' in visual mode will execute S, then {, then i
