(defun escript-all (&rest escripts)
  (progn
    (start-process-shell-command
     "escript"
	 (get-buffer-create "*somewfwe*")
     (mapconcat
      (lambda (command)
        (mapconcat
		 (lambda (cmd-sym)
		   (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
				 ((stringp cmd-sym) cmd-sym))) command " ")) escripts ";"))))

(defun escript-and (&rest escripts)
  (progn
    (start-process-shell-command
     "escript"
	 nil
     (mapconcat
      (lambda (command)
        (mapconcat
		 (lambda (cmd-sym)
		   (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
				 ((stringp cmd-sym) cmd-sym))) command " ")) escripts "&&"))))

(defun escript-output (&rest escripts)
  (progn
	(let* ((reverse-scripts (reverse escripts))
		  (no-out-commands (reverse (cdr reverse-scripts)))
		  (last-out-command (car reverse-scripts))
		  (out-buf-name (concat "escript-"(number-to-string (random)))))
	  
      (start-process-shell-command
       "escript" nil
	   (mapconcat
		(lambda (command)
		  (mapconcat
		   (lambda (cmd-sym)
			 (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
				   ((stringp cmd-sym) cmd-sym))) command " ")) no-out-commands  ";"))
	  
	  (let ((escript-proc (start-process-shell-command
						   out-buf-name
						   (get-buffer-create out-buf-name)
						   (mapconcat
							(lambda (cmd-sym)
							  (cond ((symbolp cmd-sym) (symbol-name cmd-sym))
									((stringp cmd-sym) cmd-sym))) last-out-command " "))))
		
		(while (accept-process-output escript-proc)))

	  (with-current-buffer out-buf-name
		(cdr (cdr (cdr (reverse (split-string (buffer-string) "\n")))))))))

(escript-all
 (escript-output '(ls "/bin/pwd")))

(defmacro print-escript (str)
  (print (car (eval str))))

(setq some (escript-output
  (ls "/bin/pwd")))

(provide 'elshell)
