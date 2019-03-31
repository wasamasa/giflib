(module giflib-imlib2
  (gif-imlib2-image-for-each-indexed
   gif-imlib2-image-for-each
   gif->imlib2-images)

  (import chicken scheme foreign)
  (use data-structures srfi-4 lolevel
     giflib (prefix imlib2 imlib2:))

  (include "giflib-imlib2-impl.scm"))
