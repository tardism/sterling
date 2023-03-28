
(provide 'sos-ext-mode)


;;automatically enter this mode for *.sos
(setq auto-mode-alist (cons '("\\.sos\\'" . sos-ext-mode) auto-mode-alist))


;;hook for modifying the mode without modifying the mode
(defvar sos-ext-mode-hook '()
  "*Hook for customizing SOS-Ext mode")


(defun sos-ext-mode ()
  "Mode for editing SOS modules."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `sos-ext-mode)
  (setq mode-name "Sos-Ext Module")
  ;;syntax highlighting
  (set-syntax-table sos-ext-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(sos-ext-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'sos-ext-mode-hook))



(setq sos-ext-semantic-keywords-list
      '("Module" "Builds" "on" "Judgment" "Fixed" "Translation"))
(setq sos-ext-semantic-type-list '("int" "string"))

(defvar sos-ext-font-lock-keywords
  (list
   ;;Keywords
   (cons
    (regexp-opt sos-ext-semantic-keywords-list 'words)
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
   '("Judgment[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*\\(\\([ \ta-zA-Z0-9_:,\\*\\[(|)]\\|\\]\\)+\\)"
     (1 font-lock-type-face)) ;judgment types
   '("Judgment[ \t\n]+[a-zA-Z0-9_]+[ \t\n]*:[ \t]*{\\(\\([ \t\na-zA-Z0-9_:,\\*\\[(|)]\\|\\]\\)+\\)}"
     (1 font-lock-type-face)) ;judgment types in curly braces
   '("\\([a-zA-Z0-9:_]+\\)[ ]+::=" ;defined categories
     (1 font-lock-type-face))
   '("|{\\([a-zA-Z0-9:_]+\\)}-" ;translation types
     (1 font-lock-type-face))
   ;;Rule names
   '("-----[ \t\n]*\\[\\([-a-zA-Z0-9_]+\\)\\]"
     (1 font-lock-variable-name-face))
   '("=====[ \t\n]*\\[\\([-a-zA-Z0-9_]+\\)\\]"
     (1 font-lock-variable-name-face))
   )
  )


(defvar sos-ext-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23bn" table)
    (modify-syntax-entry ?. ">" table)
    table))

