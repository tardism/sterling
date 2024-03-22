
(provide 'sterling-mode)


;;automatically enter this mode for *.sos
(setq auto-mode-alist (cons '("\\.sos\\'" . sterling-mode) auto-mode-alist))


;;hook for modifying the mode without modifying the mode
(defvar sterling-mode-hook '()
  "*Hook for customizing Sterling mode")


(defun sterling-mode ()
  "Mode for editing SOS modules."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `sterling-mode)
  (setq mode-name "Sterling Module")
  ;;syntax highlighting
  (set-syntax-table sterling-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(sterling-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'sterling-mode-hook))



(setq sterling-semantic-keywords-list
      '("Module" "Builds" "on" "Judgment" "Fixed" "Projection"))
(setq sterling-semantic-type-list '("int" "string"))

(defvar sterling-font-lock-keywords
  (list
   ;;Keywords
   (cons
    (regexp-opt sterling-semantic-keywords-list 'words)
    font-lock-keyword-face)
   (cons ; '...'  |  '~~>'  |  '|{'  |  '}-'
    "\\(\\.\\.\\.\\)\\|\\(~~>\\)\\|\\(|{\\)\\|\\(}-\\)"
    font-lock-keyword-face)
   (cons ;extensible judgment rule line
    "-----[-]*[ \t]*\\[" font-lock-keyword-face)
   '("-----[-]*[ \t]*\\[[-a-zA-Z0-9_]+\\(\\]\\)"
     (1 font-lock-keyword-face))
   (cons ;fixed judgment rule line
    "=====[=]*[ \t]*\\[" font-lock-keyword-face)
   '("=====[=]*[ \t]*\\[[-a-zA-Z0-9_]+\\(\\]\\)"
     (1 font-lock-keyword-face))
   ;;Types
   '("Judgment[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*\\(\\([ \ta-zA-Z0-9_:,\\*\\[()]\\|\\]\\)+\\)"
     (1 font-lock-type-face)) ;judgment types
   '("Projection[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*\\(\\([ \ta-zA-Z0-9_:,\\*\\[()]\\|\\]\\)+\\)"
     (1 font-lock-type-face)) ;projection types
   '("Judgment[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*{\\(\\([ \t\na-zA-Z0-9_:,\\*\\[()]\\|\\]\\)+\\)}"
     (1 font-lock-type-face)) ;judgment types in curly braces
   '("Projection[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*{\\(\\([ \t\na-zA-Z0-9_:,\\*\\[()]\\|\\]\\)+\\)}"
     (1 font-lock-type-face)) ;projection types in curly braces
   '("\\([a-zA-Z0-9:_]+\\)[ ]+::=" ;defined categories
     (1 font-lock-type-face))
   '("|{\\([a-zA-Z0-9:_]+\\)}-" ;projection types
     (1 font-lock-type-face))
   ;;Rule names
   '("-----[ \t\n]*\\[\\([-a-zA-Z0-9_]+\\)\\]"
     (1 font-lock-variable-name-face))
   '("=====[ \t\n]*\\[\\([-a-zA-Z0-9_]+\\)\\]"
     (1 font-lock-variable-name-face))
   )
  )


(defvar sterling-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23bn" table)
    (modify-syntax-entry ?. ">" table)
    table))

