(module giflib
  (open-gif gif? slurp-gif close-gif
   gif-width gif-height gif-resolution gif-bg-color
   gif-color-map color-map? color-map-count color-map-resolution
   color-map-ref color? color-red color-green color-blue)

(import chicken scheme foreign)

(foreign-declare "#include \"gif_lib.h\"")

(define-record gif pointer)
(define-record color-map pointer)
(define-record color red green blue)

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

(define (gif-resolution gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                        "C_return(gif->SColorResolution);")
       gif*))))

(define (gif-bg-color gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                        "C_return(gif->SBackGroundColor);")
       gif*))))

(define (gif-color-map gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let ((color-map* ((foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "GifFileType")) gif))
                                          "C_return(gif->SColorMap);")
                         gif*)))
        (if color-map*
            (make-color-map color-map*)
            #f)))))

(define (color-map-count color-map)
  (let ((color-map* (color-map-pointer color-map)))
    (when color-map*
      ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap))
                        "C_return(colormap->ColorCount);")
       color-map*))))

(define (color-map-resolution color-map)
  (let ((color-map* (color-map-pointer color-map)))
    (when color-map*
      ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap))
                        "C_return(colormap->BitsPerPixel);")
       color-map*))))

(define (color-map-ref color-map index)
  (let ((color-map* (color-map-pointer color-map)))
    (when color-map*
      (let ((count ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap))
                                     "C_return(colormap->ColorCount);")
                    color-map*)))
        (if (and (>= index 0) (< index count))
            (let ((red ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap)
                                              (int i))
                                            "C_return(colormap->Colors[i].Red);")
                           color-map* index))
                  (green ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap)
                                              (int i))
                                            "C_return(colormap->Colors[i].Green);")
                           color-map* index))
                  (blue ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) colormap)
                                              (int i))
                                            "C_return(colormap->Colors[i].Blue);")
                           color-map* index)))
              (make-color red green blue))
            (abort
             (make-composite-condition
              (make-property-condition
               'exn
               'message "Out of bounds")
              (make-property-condition
               'bounds))))))))

)
