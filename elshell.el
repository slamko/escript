(defmacro escript (l)
  (start-process-shell-command "no" nil
                               (mapconcat 'symbol-name l " ")))

(escript (killall pnmixer))

(provide 'elshell)
