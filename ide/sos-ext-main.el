
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



(defvar sos-ext-main-font-lock-keywords
  (list
   ;;Keywords
   (cons
    "\\(Module\\)\\|\\(Function\\)\\|\\(Print\\)\\|\\(Before\\)\\|\\(Let\\)\\|\\(In\\)\\|\\(Parse\\)\\|\\(from\\)\\|\\(If\\)\\|\\(Then\\)\\|\\(Else\\)\\|\\(Derive\\)\\|\\(for\\)\\|\\(assigning\\)\\|\\(Read\\)"
    font-lock-keyword-face)
   ;;Known types
   (cons
    "\\(int\\)\\|\\(string\\)\\|\\(list\\)\\|\\(bool\\)"
    font-lock-type-face)
    )
  )


(defvar sos-ext-main-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?* ". 23n" table)
    table))

