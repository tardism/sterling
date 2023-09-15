
(provide 'sterling-main-mode)


;;automatically enter this mode for *.main
(setq auto-mode-alist (cons '("\\.main\\'" . sterling-main-mode) auto-mode-alist))


;;hook for modifying the mode without modifying the mode
(defvar sterling-main-mode-hook '()
  "*Hook for customizing Sterling main mode")


(defun sterling-main-mode ()
  "Mode for editing SOS main modules."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `sterling-main-mode)
  (setq mode-name "Sterling Main")
  ;;syntax highlighting
  (set-syntax-table sterling-main-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(sterling-main-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'sterling-main-mode-hook))



(setq sterling-main-keyword-list
      '("Module" "Function" "Print" "Before" "Let" "In" "Parse" "from"
        "If" "Then" "Else" "Derive" "for" "assigning" "Read"))
(setq sterling-main-type-list '("int" "string" "bool"))

(defvar sterling-main-font-lock-keywords
  (list
   ;;Keywords
   (cons
    (regexp-opt sterling-main-keyword-list 'words)
    font-lock-keyword-face)
   ;;Types
   '(":[ \t\n]*\\(\\([ a-zA-Z0-9_:,\\[()]\\|\\]\\)+\\)[ \t\n]*>"
     (1 font-lock-type-face))
   '("Parse[ \t\n]+\\([a-zA-Z0-9_:]+\\)" (1 font-lock-type-face))
   '("->[ \t\n]*\\(\\([ a-zA-Z0-9_:,\\[()]\\|\\]\\)+\\){"
     (1 font-lock-type-face))
   ;;Variable names
   '("<[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*:" ;function parameters
     (1 font-lock-variable-name-face))
   '("\\(\\([a-zA-Z0-9_]+,[ \t\n]*\\)*[a-zA-Z0-9_]+\\)[ \t\n]*:="
     (1 font-lock-variable-name-face)) ;assigned in Let
   ;;Function names
   '("Function[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*:"
     (1 font-lock-function-name-face))
   )
  )


(defvar sterling-main-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23bn" table)
    (modify-syntax-entry ?. ">" table)
    table))

