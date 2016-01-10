(module giflib
  (open-gif slurp-gif close-gif
   gif-width gif-height)

(import chicken scheme foreign)

(foreign-declare "#include \"gif_lib.h\"")

(define-record gif pointer)

(define GIF-ERROR (foreign-value "GIF_ERROR" int))
(define GIF-OK (foreign-value "GIF_OK" int))

(define (gif-error status)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'message ((foreign-lambda c-string "GifErrorString" int)
               status))
    (make-property-condition
     'giflib
     'code status))))

;; TODO: find some way to clean up repetitive code
(define (close-gif gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let-location ((status int 0))
        (let ((ret ((foreign-lambda int "DGifCloseFile" (c-pointer (struct "GifFileType")) (c-pointer int))
                    gif* (location status))))
          (when (= ret GIF-ERROR)
            (gif-error status))))
      (gif-pointer-set! gif #f))))

(define (open-gif filename)
  (let-location ((status int 0))
    (let ((gif* ((foreign-lambda (c-pointer (struct "GifFileType")) "DGifOpenFileName" c-string (c-pointer int))
                 filename (location status))))
      (if gif*
          (set-finalizer! (make-gif gif*) close-gif)
          (gif-error status)))))

(define (slurp-gif gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let ((ret ((foreign-lambda int "DGifSlurp" (c-pointer (struct "GifFileType")))
                  gif*)))
        (when (= ret GIF-ERROR)
          (let ((status ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                                          "C_return(gif->Error);")
                         gif*)))
            (gif-error status)))))))

(define (gif-width gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                        "C_return(gif->SWidth);")
       gif*))))

(define (gif-height gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                        "C_return(gif->SHeight);")
       gif*))))
)
