(defmacro escript (&rest escripts)
  (progn
    (start-process-shell-command
     "no"
     (get-buffer-create "*escript-3*")
     (mapconcat
      (lambda (command)
        (mapconcat 'symbol-name command " ")) escripts " ; "))
    (sleep-for 3)
    (with-current-buffer "*escript-3*"
      (buffer-string))))

(escript
 (ls)
 (pwd))

(provide 'elshell)
