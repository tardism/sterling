
(provide 'sos-ext-main-mode)


;;automatically enter this mode for *.main
(setq auto-mode-alist (cons '("\\.main\\'" . sos-ext-main-mode) auto-mode-alist))


;;hook for modifying the mode without modifying the mode
(defvar sos-ext-main-mode-hook '()
  "*Hook for customizing SOS-Ext main mode")


(defun sos-ext-main-mode ()
  "Mode for editing SOS main modules."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `sos-ext-main-mode)
  (setq mode-name "Sos-Ext Main")
  ;;syntax highlighting
  (set-syntax-table sos-ext-main-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(sos-ext-main-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'sos-ext-main-mode-hook))



(setq sos-ext-main-keyword-list
      '("Module" "Function" "Print" "Before" "Let" "In" "Parse" "from"
        "If" "Then" "Else" "Derive" "for" "assigning" "Read"))
(setq sos-ext-main-type-list '("int" "string" "bool"))

(defvar sos-ext-main-font-lock-keywords
  (list
   ;;Keywords
   (cons
    (regexp-opt sos-ext-main-keyword-list 'words)
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


(defvar sos-ext-main-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23bn" table)
    (modify-syntax-entry ?. ">" table)
    table))

