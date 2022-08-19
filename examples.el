
(load-file "./elshell.el")
(require 'escript)

(escript-one-out
 '(tree
   (escript-one '(pwd))))

(redirect
 (escript
  ";"
  '(ls))
 "./lsout"
 t
 nil) 

(redirect-stdout
 (pipe
  (redirect-str-stderr
   (escript-one-str
    '(pwd))
   "/home/slamko/pwd")
  '(ls))
 "~/ls")

 (escript-import-env)

 (escript-one-out
  '(tree HOME))
