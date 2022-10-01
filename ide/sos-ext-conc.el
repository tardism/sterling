
(provide 'sos-ext-conc-mode)


;;automatically enter this mode for *.conc
(setq auto-mode-alist (cons '("\\.conc\\'" . sos-ext-conc-mode) auto-mode-alist))


;;hook for modifying the mode without modifying the mode
(defvar sos-ext-conc-mode-hook '()
  "*Hook for customizing SOS-Ext-Conc mode")


(defun sos-ext-conc-mode ()
  "Mode for editing SOS concrete modules."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `sos-ext-conc-mode)
  (setq mode-name "Sos-Ext Concrete Files")
  ;;syntax highlighting
  (set-syntax-table sos-ext-conc-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(sos-ext-conc-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'sos-ext-conc-mode-hook))



(defvar sos-ext-conc-font-lock-keywords
  (list
   ;;Keywords
   (cons
    "\\(Module\\)\\|\\(ignore\\)\\|\\(~~>\\)\\|\\(\\.\\.\\.\\)"
    font-lock-keyword-face)
   ;;Built-in
   (cons
    "\\($to_int\\)"
    font-lock-builtin-face)

   ;; /regex/
   (cons
    ;;regex is a copy for each delimiter
    ;;opener ( ((not (opener or newline)) or (backslash opener))
    ;;          (not (newline or backslash)) ) optional opener
    ;;matches single-line strings with backslash-escaped quotes/slashes
    (concat "\\(" ;;start forward slash group
            "/\\(\\([^/\n]\\|\\(\\\\/\\)\\)*[^\n\\\\]\\)?/"
            "\\)") ;;end forward slash group
    font-lock-string-face)
   )
  )
;;Type for concrete nonterminal
(font-lock-add-keywords 'sos-ext-conc-mode
  '(("<\\([a-zA-Z0-9_-]+\\)>" 1 font-lock-type-face t)))
;;Nonterminal name
(font-lock-add-keywords 'sos-ext-conc-mode
  '(("\\([a-zA-Z0-9_-]+\\) *<" 1 font-lock-type-face t)))
(font-lock-add-keywords 'sos-ext-conc-mode
  '(("\\([a-zA-Z0-9_-]+\\) *::=" 1 font-lock-type-face t)))
;;Terminal name
(font-lock-add-keywords 'sos-ext-conc-mode ;;override terminal name
  '(("\\(ignore\\) */.*/" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'sos-ext-conc-mode
  '(("\\([a-zA-Z0-9_-]+\\) */.*/" 1 font-lock-variable-name-face t)))
;;Production-member references
(font-lock-add-keywords 'sos-ext-conc-mode
  '(("\\(\$[0-9]+\\)" 1 font-lock-variable-name-face t)))


(defvar sos-ext-conc-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?* ". 23n" table)
    table))

