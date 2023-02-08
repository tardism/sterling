
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



(defvar sos-ext-font-lock-keywords
  (list
   ;;Keywords
   (cons
    "\\(Module\\)\\|\\(Builds\\)\\|\\(on\\)\\|\\(Judgment\\)\\|\\(Fixed\\)\\|\\(Translation\\)\\|\\(\\.\\.\\.\\)"
    font-lock-keyword-face)
   ;;Known types
   (cons
    "\\(int\\)\\|\\(string\\)"
    font-lock-type-face)
    )
  )


(defvar sos-ext-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?* ". 23n" table)
    table))

