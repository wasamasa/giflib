(module giflib-imlib2
  (gif-imlib2-image-for-each-indexed
   gif-imlib2-image-for-each
   gif->imlib2-images)

  (import scheme)
  (cond-expand
   (chicken-4
    (import chicken foreign)
    (use data-structures srfi-4 lolevel
         giflib (prefix imlib2 imlib2:)))
   (chicken-5
    (import (chicken base))
    (import (chicken bitwise))
    (import (chicken condition))
    (import (chicken fixnum))
    (import (chicken locative))
    (import (chicken type))
    (import (srfi 4))
    (import giflib)
    (import (prefix imlib2 imlib2:))))

  (include "giflib-imlib2-impl.scm"))
