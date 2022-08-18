(require 'cl-lib)
(require 'f)

(cl-defstruct proc-out
  (err "")
  (out ""))

(defvar-local escript--err-cache "/home/slamko/.cache/escript")

(defun cmd-sym-to-string (sym)
  (pcase sym
     ((pred symbolp) (symbol-name sym))
     ((pred stringp) sym)))

(defun concat-command (command)
  (mapconcat #'cmd-sym-to-string command " "))

(defun concat-command-redirect (command)
  (concat (mapconcat #'cmd-sym-to-string command " ") " 2>>/home/slamko/.cache/escript"))

(defun concat-scripts (escripts delim)
  (mapconcat #'concat-command-redirect escripts delim))

(defun read-command-buffer (buf)
  (with-current-buffer buf
    (let* ((buf-str (buffer-string))
          (buf-str-len (length buf-str)))
      (if (> buf-str-len 1)
          (substring buf-str 0 (- (length buf-str) 1))
        buf-str))))

(defun escript--run-cmd (name buf cmd)
  (make-process
   :name name
   :buffer buf
   :sentinel #'ignore
   :command `("/bin/sh" "-c" ,cmd)))
   
(defun escript (delim &rest escripts)
  (progn
    (let ((out-buf (get-buffer-create "escript-out")))
      (with-current-buffer out-buf
        (erase-buffer))
      (delete-file escript--err-cache)
      
      (let ((escript-proc
             (escript--run-cmd
              "escript" out-buf (concat-scripts escripts delim))))
        (progn
          (while (accept-process-output escript-proc))
          (make-proc-out
           :out (read-command-buffer out-buf)
           :err (f-read-text escript--err-cache)))))))
  
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

(defun redirect (proc file stdout stderr)
  (cond ((and stdout stderr)
         (progn
           (f-write-text (proc-out-out proc) 'utf-8 file)
           (f-write-text (proc-out-err proc) 'utf-8 file)
           ""))
         (stdout 
          (progn (f-write-text (proc-out-out proc) 'utf-8 file)
                 ""))
         (stderr 
          (progn (f-write-text (proc-out-err proc) 'utf-8 file)
                 (proc-out-out proc)))))

(defun redirect-stdout (proc file)
  (redirect proc file t nil))

(defun redirect-stderr (proc file)
  (redirect proc file nil t))
  
(defun escript--list-binaries (&optional bin-path)
  (escript--unique-bins
   (escript--all-bins
	(if bin-path (list bin-path) (escript--get-bin-directories)))))

(defun escript--list-env ()
  (mapcar (lambda (envv)
            (let ((env-val-l (split-string envv "=" t))) 
                  (cons (intern (car env-val-l)) (apply #'concat (cdr env-val-l))))) process-environment))

(defun escript--setq-var-name (bin-name-cell)
  (let ((bin-sym (car bin-name-cell)))
    (make-local-variable bin-sym)
    (set bin-sym (cdr bin-name-cell))))

(defun escript--define-sh-vars (binary-cells)
  (mapc #'escript--setq-var-name  binary-cells))

(defun escript-import-env ()
  (escript--define-sh-vars (escript--list-env))
  (escript--define-sh-vars (escript--list-binaries)))

(defun escript-print (str)
  (when (not (string= str "")) (princ str)))

(defun escript-printn (str)
  (princ (format "%s\n" str)))

(defun pipe-str (&rest escripts)
  (proc-out-out (apply #'pipe escripts)))

(defun escript-and-str (&rest escripts)
  (proc-out-out (apply #'escript "&&" escripts)))

(defun escript-all-str (&rest escripts)
  (apply #'escript ";" escripts))

(defun escript-one-str (&rest escripts)
  (apply #'escript-all-str escripts))

(defun pipe (&rest escripts)
  (apply #'escript "|"
         (mapcar (lambda (script)
                   (if (stringp script)
                       (list 'echo script)
                     script)) escripts)))

(defun escript-and (&rest escripts)
  (apply #'escript "&&" escripts))

(defun escript-all (&rest escripts)
  (apply #'escript ";" escripts))

(defun escript-one (&rest escripts)
  (apply #'escript-all escripts))

(defun escript-out (delim &rest escripts)
  (escript-print (apply #'escript delim escripts)))

(defun escript-pipe-out (&rest escripts)
  (escript-print (proc-out-out (apply #'pipe escripts)))
  (escript-print (proc-out-err (apply #'pipe escripts))))

(defun escript-and-out (delim &rest escripts)
  (escript-print (proc-out-out (apply #'escript "&&" escripts)))
  (escript-print (proc-out-err (apply #'escript "&&" escripts))))

(defun escript-all-out (&rest escripts)
  (escript-print (proc-out-out (apply #'escript ";" escripts)))
  (escript-print (proc-out-err (apply #'escript ";" escripts))))
  
(defun escript-one-out (&rest escripts)
  (apply #'escript-all-out escripts))

(defun val (proc)
  (proc-out-out proc))

(provide 'escript)
