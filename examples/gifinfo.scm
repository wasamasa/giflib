(use (only ports with-output-to-port)
     (rename format (format cl-format))
     (only srfi-1 append-map)
     (only posix directory? find-files)
     getopt-long
     giflib)

(define (color->hex color)
  (let ((red (color-red color))
        (green (color-green color))
        (blue (color-blue color)))
    (cl-format #f "#~2,'0x~2,'0x~2,'0x"
               red green blue)))

(define (u8vector->hex u8vector)
  (cl-format #f "~{~2,'0x~}" (u8vector->list u8vector)))

(define (gif-info file-name verbosity)
  (print file-name)
  (let ((gif (open-gif file-name)))
    (slurp-gif gif)
    (printf "Dimensions: ~a x ~a\n" (gif-width gif) (gif-height gif))
    (printf "Color resolution: ~a bits\n" (gif-resolution gif))
    (printf "Background color: ~a\n" (gif-bg-index gif))
    (let ((aspect-ratio (gif-aspect-ratio gif)))
      (if aspect-ratio
          (printf "Aspect ratio: ~a\n" aspect-ratio)
          (printf "Aspect ratio: unspecified\n")))
    (let ((color-map (gif-color-map gif)))
      (if color-map
          (begin
            (printf "Global Color Map:\n")
            (printf "  Count: ~a\n" (color-map-count color-map))
            (printf "  Resolution: ~a\n" (color-map-resolution color-map))
            (printf "  Sorted: ~a\n" (if (color-map-sorted? color-map) "Yes" "No"))
            (when (> verbosity 0)
              (printf "  Colors:\n")
              (color-map-for-each-indexed
               (lambda (color i)
                 (printf "    Color ~a: ~a\n" i (color->hex color)))
               color-map)))
          (printf "Global Color Map: absent\n")))
    (printf "Frames:\n")
    (printf "  Count: ~a\n" (gif-frame-count gif))
    (gif-frame-for-each-indexed
     (lambda (frame i)
       (printf "  Frame ~a:\n" i)
       (printf "    Frame Interlaced: ~a\n" (if (frame-interlaced? frame) "Yes" "No"))
       (printf "    Frame Left: ~a\n" (frame-left frame))
       (printf "    Frame Top: ~a\n" (frame-top frame))
       (printf "    Frame Width: ~a\n" (frame-width frame))
       (printf "    Frame Height: ~a\n" (frame-height frame))
       (let ((color-map (frame-color-map frame)))
         (if color-map
             (begin
               (printf "    Local Color Map:\n")
               (printf "      Count: ~a\n" (color-map-count color-map))
               (printf "      Resolution: ~a\n" (color-map-resolution color-map))
               (printf "      Sorted: ~a\n" (if (color-map-sorted? color-map) "Yes" "No"))
               (when (> verbosity 0)
                 (printf "      Colors:\n")
                 (color-map-for-each-indexed
                  (lambda (color i)
                    (printf "        Color ~a: ~a\n" i (color->hex color)))
                  color-map)))
             (printf "    Local Color Map: absent\n")))
       (when (> verbosity 1)
         (printf "    Image Data:\n")
         (let ((color-map (or (gif-color-map gif) (frame-color-map frame)))
               (width (frame-width frame))
               (height (frame-height frame)))
           (let loop ((y 0))
             (when (< y height)
               (let loop ((x 0))
                 (when (< x width)
                   (let* ((pixel (frame-pixel frame x y))
                          (color (color-map-ref color-map pixel))
                          (red (color-red color))
                          (green (color-green color))
                          (blue (color-blue color)))
                     #;(cl-format #t "~a|~a: ~a\n" x y pixel)
                     (cl-format #t "~a|~a: #~2,'0x~2,'0x~2,'0x (~a)\n"
                                x y red green blue pixel)
                     (loop (add1 x)))))
               (newline)
               (loop (add1 y))))))
       (let ((extension-block-count (frame-extension-block-count frame)))
         (if (not (zero? extension-block-count))
             (begin
               (printf "    Frame Extension Blocks:\n")
               (printf "      Count: ~a\n" extension-block-count)
               (frame-extension-block-for-each-indexed
                (lambda (extension-block i)
                  (printf "      Extension Block ~a:\n" i)
                  (cond
                   ((sub-block? extension-block)
                    (printf "        Type: Sub Block\n")
                    (printf "        ID: ~a\n" (sub-block-id extension-block))
                    (printf "        Data: ~a\n" (u8vector->hex (sub-block-data extension-block))))
                   ((comment-block? extension-block)
                    (printf "        Type: Comment Block\n")
                    (printf "        Text: ~a\n" (comment-block-text extension-block)))
                   ((graphics-control-block? extension-block)
                    (printf "        Type: Graphics Control Block\n")
                    (printf "        Disposal: ~a\n"
                            (string-titlecase (symbol->string (graphics-control-block-disposal extension-block))))
                    (printf "        User Input: ~a\n"
                            (if (graphics-control-block-user-input? extension-block) "Yes" "No"))
                    (printf "        Delay: ~ams\n"
                            (* (graphics-control-block-delay extension-block) 10))
                    (let ((transparency-index (graphics-control-block-transparency-index extension-block)))
                      (if transparency-index
                          (printf "        Transparency Color: ~a\n" transparency-index)
                          (printf "        Transparency Color: absent\n"))))
                   ((text-block? extension-block)
                    (printf "        Type: Text Block\n")
                    (printf "        Grid Left: ~a\n" (text-block-grid-left extension-block))
                    (printf "        Grid Top: ~a\n" (text-block-grid-top extension-block))
                    (printf "        Grid Width: ~a\n" (text-block-grid-width extension-block))
                    (printf "        Grid Height: ~a\n" (text-block-grid-height extension-block))
                    (printf "        Cell Width: ~a\n" (text-block-cell-width extension-block))
                    (printf "        Cell Height: ~a\n" (text-block-cell-height extension-block))
                    (printf "        Foreground Color: ~a\n" (text-block-fg-index extension-block))
                    (printf "        Background Color: ~a\n" (text-block-bg-index extension-block)))
                   ((application-block? extension-block)
                    (printf "        Type: Application Block\n")
                    (printf "        Identifier: ~a\n" (application-block-identifier extension-block))
                    (printf "        Auth Code: ~a\n" (application-block-auth-code extension-block)))))
                frame))
               (printf "    Frame Extension Blocks: absent\n"))))
     gif)
    (newline)))

(define usage-hint "Usage: gifinfo [option] [file ...]\n")

(define options
  '((verbose
     "Increase verbosity"
     (required #f)
     (value #f)
     (single-char #\v))
    (help
     "Prints this help"
     (required #f)
     (value #f)
     (single-char #\h))))

(define (print-error . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply print args))))

(define (args->files args)
  (append-map
   (lambda (arg)
     (if (directory? arg)
         (find-files arg test: ".*\\.gif$")
         (list arg)))
   args))

(define (main)
  (let* ((opts
          (condition-case
           (getopt-long (command-line-arguments) options)
           (e (exn)
              (error-message
               (print-error
                (format "Error: ~a: ~a\n"
                        ((condition-property-accessor 'exn 'message) e)
                        ((condition-property-accessor 'exn 'arguments) e))
                usage-hint (usage options))
               (exit 1)))))
         (help? (alist-ref 'help opts))
         (verbosity (count (lambda (item) (eqv? (car item) 'verbose)) opts))
         (args (alist-ref '@ opts)))
    (when help?
      (print-error usage-hint (usage options))
      (exit 0))
    (when (null? args)
      (print-error "No input file(s) specified\n" usage-hint)
      (exit 1))
    (for-each
     (lambda (file) (gif-info file verbosity))
     (args->files args))))

(main)
