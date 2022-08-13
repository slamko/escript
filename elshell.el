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


(defun escript--get-bin-directories ()
  (let ((path-var (getenv "PATH")))
	(when path-var
	  (split-string path-var ":"))))

(defun escript--all-bins (bin-directories)
  (let (all-bin-names)
	(dolist (bin-path bin-directories all-bin-names)
	  (if (file-exists-p bin-path)
		  (setq all-bin-names (append (directory-files bin-path t "^[a-zA-Z0-9].*$") all-bin-names))))))

(defun escript--unique-bins (bins)
  (let (unique-bin-names)
	(dolist (bin-path bins unique-bin-names)
	  (let ((bin-symbol (intern (file-name-nondirectory bin-path))))
		(if (and
             (not (assq bin-symbol unique-bin-names))
             (file-executable-p bin-path) (not (file-directory-p bin-path)))
			(setq unique-bin-names (push (cons bin-symbol bin-path) unique-bin-names)))))))

(defun escript--list-binaries (&optional bin-path)
  (escript--unique-bins
   (escript--all-bins
	(if bin-path (list bin-path) (escript--get-bin-directories)))))

(defun escript--setq-bin-name (bin-name-cell)
  (set (car bin-name-cell) (cdr bin-name-cell)))

(defun escript--define-bin-vars (binary-cells)
  (mapc 'escript--setq-bin-name  binary-cells))

(escript--define-bin-vars (escript--list-binaries))

(escript-all
 (escript-output '(acceleration_speed))
 '(emacs-28.1)
 '(MagickCore-config))

(provide 'escript)
