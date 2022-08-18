

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

(escript-pipe-out
 "ls"
 '(cat))

 (escript-import-env)

 (escript-one-out
  '(tree HOME))
