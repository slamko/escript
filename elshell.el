(defmacro escript (&rest escripts)
  (progn
    (start-process-shell-command
     "escript"
	 (get-buffer "*Messages*")
     (mapconcat
      (lambda (command)
        (mapconcat
		 (lambda (cmd-sym)
		   (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
				 ((stringp cmd-sym) cmd-sym))) command " ")) escripts " ; "))))

(defmacro escript-output (&rest escripts)
  (progn
	(let* ((reverse-scripts (reverse escripts))
		  (no-out-commands (reverse (cdr reverse-scripts)))
		  (last-out-command (car reverse-scripts))
		  (out-buf-name (concat "escript-"(number-to-string (random)))))
	  
      (start-process-shell-command
       "escript" nil (mapconcat
					  (lambda (command)
						(mapconcat (lambda (cmd-sym)
		   (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
				 ((stringp cmd-sym) cmd-sym))) command " ")) no-out-commands  " ; "))
	  
	  (start-process-shell-command
       out-buf-name
       (get-buffer-create out-buf-name)
       (mapconcat 'symbol-name last-out-command " "))

	  (sleep-for 1)
	  (with-current-buffer out-buf-name
		(cdr (cdr (cdr (reverse (split-string (buffer-string) "\n")))))))))

(escript
 (ls "/"))
 
(escript
 (escript-output
  (ls "/bin/pwd")))

(provide 'elshell)
