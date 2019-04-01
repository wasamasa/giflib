(import scheme)
(cond-expand
 (chicken-4
  (use giflib giflib-imlib2 imlib2 srfi-13))
 (chicken-5
  (import (chicken format))
  (import (chicken process-context))
  (import (srfi 13))
  (import giflib)
  (import giflib-imlib2)
  (import imlib2)))

(let* ((input-file (car (command-line-arguments)))
       (gif (open-gif input-file)))
  (slurp-gif gif)
  (let* ((frame-count (gif-frame-count gif))
         (format-width (max 2 (string-length (number->string frame-count)))))
    (gif-imlib2-image-for-each-indexed
     (lambda (image i)
       (let* ((number (string-pad (number->string (add1 i)) format-width #\0))
              (filename (format "frame-~a.png" number)))
         (image-save image filename)))
     gif))
  (close-gif gif))
