# -*- mode: snippet -*-
# name: source block
# key: <s
# --
#+BEGIN_SRC ${1:$$(yas-auto-next (yas-completing-read "Language: " (mapcar 'car org-babel-load-languages))) }
$0
#+END_SRC
