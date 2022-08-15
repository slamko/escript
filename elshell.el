(require 'cl-lib)

(cl-defstruct proc-out
  (err "")
  (out ""))

(defun cmd-sym-to-string (sym)
  (pcase sym
     ((pred symbolp) (symbol-name sym))
     ((pred stringp) sym)))

(defun concat-command (command)
  (mapconcat #'cmd-sym-to-string command " "))

(defun concat-scripts (escripts delim)
  (mapconcat #'concat-command escripts delim))

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
   :command `("/bin/sh" "-c" ,cmd)
   :connection-type 'pipe
   :stderr (get-buffer-create "escript-err")))
   
(defun escript (delim &rest escripts)
  (progn
    (let ((out-buf (get-buffer-create "escript-out"))
          (err-buf (get-buffer-create "escript-err")))
      (with-current-buffer out-buf
        (erase-buffer))
      (with-current-buffer err-buf
        (erase-buffer))
      
      (let ((escript-proc
             (escript--run-cmd
              "escript" out-buf (concat-scripts escripts delim))))
        (progn
          (while (accept-process-output escript-proc))
          (make-proc-out
           :out (read-command-buffer out-buf)
           :err (read-command-buffer err-buf)))))))

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

(defun redirect (escripts-l file stdout stderr)
  (let ((out-buf (get-buffer-create "escript-out")))
      (with-current-buffer out-buf
        (erase-buffer))
      
      (let ((escript-proc
             (escript--run-cmd
              "escript" out-buf
              (concat (concat-scripts escripts-l delim)
                      (concat
                       (cond ((and stdout stderr) "1>&2")
                             (stdout ">")
                             (stderr "2>")) file)))))
        (progn
          (while (accept-process-output escript-proc))
          (read-command-buffer out-buf)))))
  

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
  (princ str))

(defun escript-printn (str)
  (princ (format "%s\n" str)))

(defun escript-pipe (&rest escripts)
  (apply #'escript "|" escripts))

(defun escript-and (&rest escripts)
  (apply #'escript "&&" escripts))

(defun escript-all (&rest escripts)
  (apply #'escript ";" escripts))

(defun escript-one (&rest escripts)
  (apply #'escript-all escripts))

(defun escript-out (delim &rest escripts)
  (escript-print (apply #'escript delim escripts)))

(defun escript-pipe-out (&rest escripts)
  (escript-print (apply #'escript-pipe escripts)))

(defun escript-and-out (delim &rest escripts)
  (escript-print (apply #'escript-and escripts)))

(defun escript-all-out (&rest escripts)
  (escript-print (apply #'escript-all escripts)))
  
(defun escript-one-out (&rest escripts)
  (apply #'escript-all-out escripts))

(escript-one-out
 '(tree
   (escript-one '(pwd))))

(redirect
 '(ls)
 "./lsout"
 t
 nil)

(escript-pipe-out
 '(ls))

(escript-import-env)

(escript-one-out
 '(tree XDG_RUNTIME_DIR))

(provide 'escript)
