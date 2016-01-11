(module giflib
  (open-gif gif? slurp-gif close-gif
   gif-width gif-height gif-resolution gif-bg-color
   gif-color-map color-map? color-map-resolution
   color-map-count color-map-ref color? color-red color-green color-blue
   gif-frame-count gif-frame-ref frame? frame-width frame-height frame-left frame-top frame-interlaced? frame-color-map frame-pixel)

(import chicken scheme foreign)

(foreign-declare "#include \"gif_lib.h\"")

(define-record gif pointer)
(define-record frame pointer)
(define-record color-map pointer)
(define-record color red green blue)

(define GIF-ERROR (foreign-value "GIF_ERROR" bool))
(define GIF-OK (foreign-value "GIF_OK" bool))

(define (gif-error status location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message ((foreign-lambda c-string "GifErrorString" int)
               status))
    (make-property-condition
     'giflib
     'code status))))

(define (oob-error index count location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message (format "Out of bounds: ~a / ~a" index count))
    (make-property-condition
     'bounds))))

;; TODO: find some way to clean up repetitive code
(define (close-gif gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let-location ((status int 0))
        (let ((ret ((foreign-lambda int "DGifCloseFile" (c-pointer (struct "GifFileType")) (c-pointer int))
                    gif* (location status))))
          (when (= ret GIF-ERROR)
            (gif-error status 'close-gif))))
      (gif-pointer-set! gif #f))))

(define (open-gif filename)
  (let-location ((status int 0))
    (let ((gif* ((foreign-lambda (c-pointer (struct "GifFileType")) "DGifOpenFileName" c-string (c-pointer int))
                 filename (location status))))
      (if gif*
          (set-finalizer! (make-gif gif*) close-gif)
          (gif-error status 'open-gif)))))

(define (slurp-gif gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let ((ret ((foreign-lambda int "DGifSlurp" (c-pointer (struct "GifFileType")))
                  gif*)))
        (when (= ret GIF-ERROR)
          (let ((status ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                                          "C_return(gif->Error);")
                         gif*)))
            (gif-error status 'slurp-gif)))))))

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
                                          "struct ColorMapObject *colormap = gif->SColorMap;
                                           if(colormap) C_return(&colormap); else C_return(false);")
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
            (let ((red ((foreign-lambda* byte (((c-pointer (struct "ColorMapObject")) colormap)
                                               (int i))
                                         "C_return(colormap->Colors[i].Red);")
                           color-map* index))
                  (green ((foreign-lambda* byte (((c-pointer (struct "ColorMapObject")) colormap)
                                                 (int i))
                                           "C_return(colormap->Colors[i].Green);")
                           color-map* index))
                  (blue ((foreign-lambda* byte (((c-pointer (struct "ColorMapObject")) colormap)
                                                (int i))
                                          "C_return(colormap->Colors[i].Blue);")
                           color-map* index)))
              (make-color red green blue))
            (oob-error index count 'color-map-ref))))))

(define (gif-frame-count gif)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                        "C_return(gif->ImageCount);")
       gif*))))

(define (gif-frame-ref gif index)
  (let ((gif* (gif-pointer gif)))
    (when gif*
      (let ((count ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                                     "C_return(gif->ImageCount);")
                    gif*)))
        (if (and (>= index 0) (< index count))
            (let ((frame* ((foreign-lambda* (c-pointer (struct "SavedImage")) (((c-pointer (struct "GifFileType")) gif)
                                                                               (int i))
                                            "C_return(&(gif->SavedImages[i]));")
                           gif* index)))
              (if frame*
                  (make-frame frame*)
                  #f))
            (oob-error index count 'gif-frame-ref))))))

(define (frame-width frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                        "C_return(frame->ImageDesc.Width);")
       frame*))))

(define (frame-height frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                        "C_return(frame->ImageDesc.Height);")
       frame*))))

(define (frame-left frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                        "C_return(frame->ImageDesc.Left);")
       frame*))))

(define (frame-top frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                        "C_return(frame->ImageDesc.Top);")
       frame*))))

(define (frame-interlaced? frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      ((foreign-lambda* bool (((c-pointer (struct "SavedImage")) frame))
                        "C_return(frame->ImageDesc.Interlace);")
       frame*))))

(define (frame-color-map frame)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      (let ((color-map* ((foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "SavedImage")) frame))
                                          "struct ColorMapObject *colormap = frame->ImageDesc.ColorMap;
                                           if(colormap) C_return(&colormap); else C_return(false);")
                         frame*)))
        (if color-map*
            (make-color-map color-map*)
            #f)))))

;; TODO: go for more intuitive semantics?
(define (frame-pixel frame x y)
  (let ((frame* (frame-pointer frame)))
    (when frame*
      (let ((width ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                                     "C_return(frame->ImageDesc.Width);")
                    frame*))
            (height ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                                      "C_return(frame->ImageDesc.Height);")
                     frame*)))
        (if (and (>= x 0) (>= y 0)
                 (< x width) (< y height))
            ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)
                                   (int width)
                                   (int x)
                                   (int y))
                              "C_return(frame->RasterBits[y*width+x]);")
             frame* width x y)
            (oob-error (format "~a|~a" x y) (format "~a:~a" width height) 'frame-pixel))))))

)
