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
           (vector-set! palette i #f)
           (let ((r (color-red color))
                 (g (color-green color))
                 (b (color-blue color)))
             (vector-set! palette i (argb->uint32 255 r g b)))))
     color-map)
    palette))

(define (%gif-imlib2-image-fold kons knil gif location)
  (let* ((global-color-map (gif-color-map gif))
         (width (gif-width gif))
         (height (gif-height gif))
         (frame-count (gif-frame-count gif))
         (bg-index (gif-bg-index gif))
         (size (* width height))
         (data (make-u32vector size))
         (last #f))
    (let loop ((i 0)
               (image #f)
               (acc knil))
      (if (< i frame-count)
          (let* ((frame (gif-frame-ref gif i))
                 (pixels (frame-pixels frame))
                 (l (frame-left frame))
                 (t (frame-top frame))
                 (w (frame-width frame))
                 (h (frame-height frame))
                 (hole? (or (> l 0) (< (+ l w) width)
                            (> t 0) (< (+ t h) height)))
                 (local-color-map (frame-color-map frame))
                 (color-map (or local-color-map global-color-map))
                 (metadata (frame-metadata frame))
                 (disposal (alist-ref 'disposal metadata))
                 (transparency-index (alist-ref 'transparency-index metadata))
                 (palette (if color-map (color-map->palette color-map transparency-index) #f)))
            (when (not palette)
              (decoding-error "No palette specified" location))
            (when (> (+ l w) width)
              (decoding-error "Frame width overflow" location))
            (when (> (+ t h) height)
              (decoding-error "Frame height overflow" location))
            (when hole?
              (when (and (not disposal) (not (disposal-strategy)))
                (decoding-error "No disposal specified for empty pixels" location))
              (if (eqv? disposal 'background)
                  (let ((bg (vector-ref palette bg-index)))
                    (set! data (make-u32vector size bg)))
                  (begin
                    (when (not last)
                      (decoding-error "No previous frame encountered" location))
                    (set! data (subu32vector last 0 (u32vector-length last))))))

            (do ((y 0 (fx+ y 1)))
                ((fx= y h))
              (do ((x 0 (fx+ x 1)))
                  ((fx= x w))
                (let* ((index (fx+ (fx* (fx+ y t) width) (fx+ x l)))
                       (pixel (u8vector-ref pixels (fx+ (fx* y w) x)))
                       (color (vector-ref (the vector palette) pixel)))
                  (when color
                    (u32vector-set! data index color)))))

            (when (not (eqv? disposal 'last))
              (set! last data))
            (let ((image (imlib2:image-create-using-copied-data
                          width height
                          (make-locative (u32vector->blob/shared data)))))
              (loop (add1 i) image (kons image acc))))
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
