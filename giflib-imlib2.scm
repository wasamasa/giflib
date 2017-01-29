(module giflib-imlib2
  (gif-imlib2-image-for-each-indexed
   gif-imlib2-image-for-each
   gif->imlib2-images)

(import chicken scheme foreign)

;; HACK: used for accessors only
(define-record gif mode slurped? pointer)

(use data-structures srfi-4 lolevel
     giflib (prefix imlib2 imlib2:))

(define (define-error location message . condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (decoding-error message location)
  (define-error location message 'decoding))

(define (usage-error message location)
  (define-error location message 'usage))

(define (assert-gif-slurped! gif location)
  (when (and (eq? (gif-mode gif) 'read)
             (not (gif-slurped? gif)))
    (abort (usage-error "Gif not slurped yet" location))))

(define (argb->uint32 a r g b)
  (bitwise-ior
   (arithmetic-shift a 24)
   (arithmetic-shift r 16)
   (arithmetic-shift g 8)
   b))

(define (color-map->palette color-map transparency-index)
  (let* ((count (color-map-count color-map))
         (palette (make-vector count)))
    (color-map-for-each-indexed
     (lambda (color i)
       (if (and transparency-index (= i transparency-index))
           (vector-set! palette i 0)
           (let ((r (color-red color))
                 (g (color-green color))
                 (b (color-blue color)))
             (vector-set! palette i (argb->uint32 255 r g b)))))
     color-map)
    palette))

(define (%gif-imlib2-image-fold kons knil gif location)
  ;; algorithm adapted from sxiv
  (let* ((global-color-map (gif-color-map gif))
         (width (gif-width gif))
         (height (gif-height gif))
         (frame-count (gif-frame-count gif))
         (bg-index (gif-bg-index gif))
         (size (* width height)))
    (let loop ((index 0)
               (image #f)
               (acc knil)

               (last-x #f)
               (last-y #f)
               (last-w #f)
               (last-h #f)
               (last-disposal #f)
               (last-data #f))
      (if (< index frame-count)
          (let* ((frame (gif-frame-ref gif index))
                 (local-color-map (frame-color-map frame))
                 (color-map (or local-color-map global-color-map))
                 (pixels (frame-pixels frame))
                 (data (make-u32vector size))
                 (x (frame-left frame))
                 (y (frame-top frame))
                 (w (frame-width frame))
                 (h (frame-height frame))
                 (metadata (frame-metadata frame))
                 (disposal (alist-ref 'disposal metadata))
                 (transparency-index (alist-ref 'transparency-index metadata))
                 (palette (if color-map (color-map->palette color-map transparency-index) #f))
                 (bg-color (if color-map (vector-ref palette bg-index))))
            (when (not palette)
              (decoding-error "No palette specified" location))
            (when (> (+ x w) width)
              (decoding-error "Frame width overflow" location))
            (when (> (+ y h) height)
              (decoding-error "Frame height overflow" location))

            (do ((i 0 (fx+ i 1)))
                ((fx= i height))
              (do ((j 0 (fx+ j 1)))
                  ((fx= j width))
                (let* ((data-index (fx+ (fx* i width) j))
                       (pixels-index (fx+ (fx* (fx- i y) w) (fx- j x)))
                       (transparent? (and transparency-index
                                          (= (u8vector-ref pixels pixels-index)
                                             transparency-index))))
                  (if (or (fx< i y) (fx>= i (fx+ y h))
                          (fx< j x) (fx>= j (fx+ x w))
                          transparent?)
                      (if (and last-data
                               (or (not (eq? last-disposal 'background))
                                   (fx< i last-y) (fx>= i (fx+ last-y last-h))
                                   (fx< j last-x) (fx>= j (fx+ last-x last-w))))
                          (u32vector-set! data data-index
                                          (u32vector-ref last-data data-index))
                          (if transparent?
                              (u32vector-set! data data-index 0)
                              (u32vector-set! data data-index bg-color)))
                      (u32vector-set! data data-index
                                      (vector-ref (the vector palette)
                                                  (u8vector-ref pixels pixels-index)))))))

            (let ((image (imlib2:image-create-using-copied-data
                          width height
                          (make-locative (u32vector->blob/shared data)))))
              (when transparency-index
                (imlib2:image-alpha-set! image #t))
              (loop (add1 index) image (kons image acc)
                    x y w h disposal
                    (if (not (eq? disposal 'previous)) data last-data))))
          acc))))

(define (gif-imlib2-image-for-each proc gif)
  (assert-gif-slurped! gif 'gif-imlib2-image-for-each)
  (%gif-imlib2-image-fold
   (lambda (image _acc)
     (proc image)
     #f)
   #f gif 'gif-imlib2-image-for-each))

(define (gif-imlib2-image-for-each-indexed proc gif)
  (assert-gif-slurped! gif 'gif-imlib2-image-for-each-indexed)
  (%gif-imlib2-image-fold
   (lambda (image i)
     (proc image i)
     (add1 i))
   0 gif 'gif-imlib2-image-for-each-indexed)
  #f)

(define (gif->imlib2-images gif)
  (assert-gif-slurped! gif 'gif->imlib2-images)
  (reverse
   (%gif-imlib2-image-fold
    (lambda (image images)
      (cons image images))
    '() gif 'gif->imlib2-images)))

)
