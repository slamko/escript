
(defun cmd-sym-to-string (sym)
  (pcase sym
    ((pred symbolp) (symbol-name sym))
    ((pred stringp) sym)))

(defun concat-command (command)
  (mapconcat 'cmd-sym-to-string command " "))

(defun concat-scripts (escripts delim)
  (mapconcat 'concat-command escripts delim))

(defun read-command-buffer (buf)
  (with-current-buffer buf
    (let* ((buf-str (buffer-string))
          (buf-str-len (length buf-str)))
      (if (> buf-str-len 1)
          (substring buf-str 0 (- (length buf-str) 1))
        buf-str))))

(defun escript--run-cmd (name buf cmd)
  (let ((proc (start-process-shell-command name buf cmd)))
    (set-process-sentinel proc #'ignore)
    proc))

(defun escript (delim &rest escripts)
  (progn
    (let ((out-buf (get-buffer-create "escript-out")))
      (with-current-buffer out-buf
        (erase-buffer))
      
      (let ((escript-proc
             (escript--run-cmd
              "escript" out-buf (concat-scripts escripts delim))))
        (progn
          (while (accept-process-output escript-proc))
          (read-command-buffer out-buf))))))

(defun escript-last (&optional delim &rest escripts)
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
				   ((stringp cmd-sym) cmd-sym))) command " ")) no-out-commands (or delim ";")))
	  
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
  (let ((bin-sym (car bin-name-cell)))
    (make-local-variable bin-sym)
    (set bin-sym (cdr bin-name-cell))))

(defun escript--define-bin-vars (binary-cells)
  (mapc 'escript--setq-bin-name  binary-cells))

(defmacro escript-bin-names ()
  (escript--define-bin-vars (escript--list-binaries)))

(defun escript-pipe (&rest escripts)
  (escript "|" escripts))

(defun escript-out (delim &rest escripts)
  (print (escript ";" escripts)))
  
(print (escript ";" '(ls)))
(escript-out ";" '(ls))

(escript-pipe
 '(ls)
 '(cat))

(escript-out ";" '(pwd))

(provide 'escript)
