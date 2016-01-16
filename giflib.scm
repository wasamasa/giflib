(module giflib
  (open-gif gif? slurp-gif close-gif
   gif-width gif-height gif-resolution gif-bg-index
   gif-color-map color-map? color-map-resolution
   color-map-count color-map-ref color? color-red color-green color-blue
   gif-extension-block-count gif-extension-block-ref
   gif-frame-count gif-frame-ref frame? frame-width frame-height frame-left frame-top frame-interlaced? frame-color-map frame-pixel
   frame-extension-block-count frame-extension-block-ref
   extension-block? extension-block-type extension-block->u8vector
   extension-block->sub-block sub-block? sub-block-id sub-block-data
   extension-block->comment-block comment-block? comment-block-text
   extension-block->graphics-control-block graphics-control-block? graphics-control-block-disposal graphics-control-block-user-input? graphics-control-block-delay graphics-control-block-transparency-index
   extension-block->text-block text-block? text-block-grid-left text-block-grid-top text-block-grid-width text-block-grid-height text-block-cell-width text-block-cell-height text-block-fg-index text-block-bg-index
   extension-block->application-block application-block? application-block-identifier application-block-auth-code)

(import chicken scheme foreign)
(use srfi-4 bitstring)

(foreign-declare "#include <gif_lib.h>")

(define-record gif pointer)
(define-record frame pointer)
(define-record color-map pointer)
(define-record color red green blue)
(define-record extension-block pointer type data-length data-pointer)
(define-record sub-block id data)
(define-record comment-block text)
(define-record graphics-control-block disposal user-input? delay transparency-index)
;; see also http://www.vurdalakov.net/misc/gif
(define-record application-block identifier auth-code)
(define-record text-block grid-left grid-top grid-width grid-height cell-width cell-height fg-index bg-index)

(define GIF-ERROR (foreign-value "GIF_ERROR" byte))
(define GIF-OK (foreign-value "GIF_OK" byte))

(define CONTINUE-EXT-FUNC-CODE (foreign-value "CONTINUE_EXT_FUNC_CODE" unsigned-byte))
(define COMMENT-EXT-FUNC-CODE (foreign-value "COMMENT_EXT_FUNC_CODE" unsigned-byte))
(define GRAPHICS-EXT-FUNC-CODE (foreign-value "GRAPHICS_EXT_FUNC_CODE" unsigned-byte))
(define PLAINTEXT-EXT-FUNC-CODE (foreign-value "PLAINTEXT_EXT_FUNC_CODE" unsigned-byte))
(define APPLICATION-EXT-FUNC-CODE (foreign-value "APPLICATION_EXT_FUNC_CODE" unsigned-byte))

(define DISPOSAL-UNSPECIFIED (foreign-value "DISPOSAL_UNSPECIFIED" unsigned-byte))
(define DISPOSE-DO-NOT (foreign-value "DISPOSE_DO_NOT" unsigned-byte))
(define DISPOSE-BACKGROUND (foreign-value "DISPOSE_BACKGROUND" unsigned-byte))
(define DISPOSE-PREVIOUS (foreign-value "DISPOSE_PREVIOUS" unsigned-byte))

(define (gif-error status location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message ((foreign-lambda c-string "GifErrorString" byte)
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

(define (unknown-extension-block-error location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message "Unknown extension block")
    (make-property-condition
     'match))))

(define (unknown-disposal-error location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message "Unknown disposal")
    (make-property-condition
     'match))))

(define (unpack-error location)
  (abort
   (make-composite-condition
    (make-property-condition
     'exn
     'location location
     'message "Unpacking error")
    (make-property-condition
     'match))))

;; TODO: find some way to clean up repetitive code
(define (close-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let-location ((status int 0))
      (let ((ret ((foreign-lambda int "DGifCloseFile" (c-pointer (struct "GifFileType")) (c-pointer int))
                  gif* (location status))))
        (when (= ret GIF-ERROR)
          (gif-error status 'close-gif))))
    (gif-pointer-set! gif #f)))

(define (open-gif filename)
  (let-location ((status int 0))
    (let ((gif* ((foreign-lambda (c-pointer (struct "GifFileType")) "DGifOpenFileName" c-string (c-pointer int))
                 filename (location status))))
      (if gif*
          (set-finalizer! (make-gif gif*) close-gif)
          (gif-error status 'open-gif)))))

(define (slurp-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((ret ((foreign-lambda int "DGifSlurp" (c-pointer (struct "GifFileType")))
                gif*)))
      (when (= ret GIF-ERROR)
        (let ((status ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                                        "C_return(gif->Error);")
                       gif*)))
          (gif-error status 'slurp-gif))))))

(define (gif-width gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->SWidth);")
     gif*)))

(define (gif-height gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->SHeight);")
     gif*)))

(define (gif-resolution gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->SColorResolution);")
     gif*)))

(define (gif-bg-index gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->SBackGroundColor);")
     gif*)))

(define (gif-color-map gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((color-map* ((foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "GifFileType")) gif))
                                        "struct ColorMapObject *colormap = gif->SColorMap;
                                         if(colormap) C_return(colormap); else C_return(false);")
                       gif*)))
      (if color-map*
          (make-color-map color-map*)
          #f))))

(define (color-map-count color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) color_map))
                      "C_return(color_map->ColorCount);")
     color-map*)))

(define (color-map-resolution color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) color_map))
                      "C_return(color_map->BitsPerPixel);")
     color-map*)))

(define (color-map-ref color-map index)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (let ((count ((foreign-lambda* int (((c-pointer (struct "ColorMapObject")) color_map))
                                   "C_return(color_map->ColorCount);")
                  color-map*)))
      (if (and (>= index 0) (< index count))
          (let ((red ((foreign-lambda* unsigned-byte (((c-pointer (struct "ColorMapObject")) color_map)
                                                      (int i))
                                       "C_return(color_map->Colors[i].Red);")
                      color-map* index))
                (green ((foreign-lambda* unsigned-byte (((c-pointer (struct "ColorMapObject")) color_map)
                                                        (int i))
                                         "C_return(color_map->Colors[i].Green);")
                        color-map* index))
                (blue ((foreign-lambda* unsigned-byte (((c-pointer (struct "ColorMapObject")) color_map)
                                                       (int i))
                                        "C_return(color_map->Colors[i].Blue);")
                       color-map* index)))
            (make-color red green blue))
          (oob-error index count 'color-map-ref)))))

(define (gif-frame-count gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->ImageCount);")
     gif*)))

(define (gif-frame-ref gif index)
  (and-let* ((gif* (gif-pointer gif)))
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
          (oob-error index count 'gif-frame-ref)))))

(define (gif-extension-block-count gif)
  (and-let* ((gif* (gif-pointer gif)))
    ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                      "C_return(gif->ExtensionBlockCount);")
     gif*)))

(define (gif-extension-block-ref gif index)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count ((foreign-lambda* int (((c-pointer (struct "GifFileType")) gif))
                                  "C_return(gif->ExtensionBlockCount);")
                  gif*)))
      (if (and (>= index 0) (< index count))
          (let* ((extension-block* ((foreign-lambda* (c-pointer (struct "ExtensionBlock")) (((c-pointer (struct "GifFileType")) gif)
                                                                                            (int i))
                                                     "C_return(&(gif->ExtensionBlocks[i]));")
                                    gif* index))
                 (function ((foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block))
                                             "C_return(extension_block->Function);")
                            extension-block*))
                 (type (select function
                         ((CONTINUE-EXT-FUNC-CODE) 'sub-block)
                         ((COMMENT-EXT-FUNC-CODE) 'comment-block)
                         ((GRAPHICS-EXT-FUNC-CODE) 'graphics-control-block)
                         ((PLAINTEXT-EXT-FUNC-CODE) 'text-block)
                         ((APPLICATION-EXT-FUNC-CODE) 'application-block)
                         (else (unknown-extension-block-error 'gif-extension-block-ref))))
                 (data-length ((foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block))
                                                "C_return(extension_block->ByteCount);")
                               extension-block*))
                 (data-pointer ((foreign-lambda* (c-pointer unsigned-byte) (((c-pointer (struct "ExtensionBlock")) extension_block))
                                                 "C_return(extension_block->Bytes);")
                                extension-block*)))
             (make-extension-block extension-block* type data-length data-pointer))
          (oob-error index count 'gif-extension-block-ref)))))

(define (frame-width frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ImageDesc.Width);")
     frame*)))

(define (frame-height frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ImageDesc.Height);")
     frame*)))

(define (frame-left frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ImageDesc.Left);")
     frame*)))

(define (frame-top frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ImageDesc.Top);")
     frame*)))

(define (frame-interlaced? frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* bool (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ImageDesc.Interlace);")
     frame*)))

(define (frame-color-map frame)
  (and-let* ((frame* (frame-pointer frame)))
    (let ((color-map* ((foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "SavedImage")) frame))
                                        "struct ColorMapObject *colormap = frame->ImageDesc.ColorMap;
                                         if(colormap) C_return(colormap); else C_return(false);")
                       frame*)))
      (if color-map*
          (make-color-map color-map*)
          #f))))

(define (frame-extension-block-count frame)
  (and-let* ((frame* (frame-pointer frame)))
    ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                      "C_return(frame->ExtensionBlockCount);")
     frame*)))

(define (frame-extension-block-ref frame index)
  (and-let* ((frame* (frame-pointer frame)))
    (let ((count ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                                   "C_return(frame->ExtensionBlockCount);")
                  frame*)))
      (if (and (>= index 0) (< index count))
          (let* ((extension-block* ((foreign-lambda* (c-pointer (struct "ExtensionBlock")) (((c-pointer (struct "SavedImage")) frame)
                                                                                            (int i))
                                                     "C_return(&(frame->ExtensionBlocks[i]));")
                                    frame* index))
                 (function ((foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block))
                                             "C_return(extension_block->Function);")
                            extension-block*))
                 (type (select function
                         ((CONTINUE-EXT-FUNC-CODE) 'sub-block)
                         ((COMMENT-EXT-FUNC-CODE) 'comment-block)
                         ((GRAPHICS-EXT-FUNC-CODE) 'graphics-control-block)
                         ((PLAINTEXT-EXT-FUNC-CODE) 'text-block)
                         ((APPLICATION-EXT-FUNC-CODE) 'application-block)
                         (else (unknown-extension-block-error 'frame-extension-block-ref))))
                 (data-length ((foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block))
                                                "C_return(extension_block->ByteCount);")
                               extension-block*))
                 (data-pointer ((foreign-lambda* (c-pointer unsigned-byte) (((c-pointer (struct "ExtensionBlock")) extension_block))
                                                 "C_return(extension_block->Bytes);")
                                extension-block*)))
             (make-extension-block extension-block* type data-length data-pointer))
          (oob-error index count 'frame-extension-block-ref)))))

;; TODO: implement gif-frame-fold with more intuitive semantics
;; NOTE: https://github.com/muennich/sxiv/blob/master/image.c#L147-L155
(define (frame-pixel frame x y)
  (and-let* ((frame* (frame-pointer frame)))
    (let ((width ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                                   "C_return(frame->ImageDesc.Width);")
                  frame*))
          (height ((foreign-lambda* int (((c-pointer (struct "SavedImage")) frame))
                                    "C_return(frame->ImageDesc.Height);")
                   frame*)))
      (if (and (>= x 0) (>= y 0)
               (< x width) (< y height))
          ((foreign-lambda* unsigned-byte (((c-pointer (struct "SavedImage")) frame)
                                           (int width)
                                           (int x)
                                           (int y))
                            "C_return(frame->RasterBits[y*width+x]);")
           frame* width x y)
          (oob-error (format "~a|~a" x y) (format "~ax~a" width height) 'frame-pixel)))))

;; (define (frame-pixel-row frame row) ...)
;; (define (frame-pixel-rect frame x y width height) ...)
;; (define (frame-pixels frame) ...)

(define (extension-block->u8vector extension-block)
  (and-let* ((extension-block* (extension-block-pointer extension-block)))
    (let* ((data-pointer (extension-block-data-pointer extension-block))
           (data-length (extension-block-data-length extension-block))
           (data (make-u8vector data-length 0)))
      ((foreign-lambda* void ((u8vector dest)
                              ((c-pointer unsigned-byte) src)
                              (int size))
                        "memcpy(dest, src, size * sizeof(unsigned char));")
       data data-pointer data-length)
      data)))

;; TODO: error out on unexpected extension block type?
(define (extension-block->sub-block extension-block)
  (and-let* ((data (extension-block->u8vector extension-block)))
    (bitmatch data
      (((id 8 little)
        (data bitstring))
       (make-sub-block id (bitstring->u8vector data 8)))
      (else
       (unpack-error 'extension-block->sub-block)))))

(define (extension-block->comment-block extension-block)
  (and-let* ((data (extension-block->u8vector extension-block)))
    (make-comment-block (blob->string (u8vector->blob data)))))

(define (extension-block->graphics-control-block extension-block)
  (and-let* ((data (extension-block->u8vector extension-block)))
    (bitmatch data
      (((reserved 3 little)
        (disposal 3 little)
        (user-input? 1 boolean little)
        (transparency-index? 1 boolean little)
        (delay-time (* 2 8) little) ; hundredths of seconds
        (transparency-index 8 little)) ; index
       (make-graphics-control-block
        (select disposal
          ((DISPOSAL-UNSPECIFIED) 'unspecified)
          ((DISPOSE-DO-NOT) 'none)
          ((DISPOSE-BACKGROUND) 'background)
          ((DISPOSE-PREVIOUS) 'previous)
          (else (unknown-disposal-error 'extension-block->graphics-control-block)))
        user-input? delay-time (and transparency-index? transparency-index)))
      (else
       (unpack-error 'extension-block->graphics-control-block)))))

(define (extension-block->text-block extension-block)
  (and-let* ((data (extension-block->u8vector extension-block)))
    (bitmatch data
      (((grid-left (* 2 8) little)
        (grid-top (* 2 8) little)
        (grid-width (* 2 8) little)
        (grid-height (* 2 8) little)
        (cell-width 8 little)
        (cell-height 8 little)
        (fg-index 8 little)
        (bg-index 8 little))
       (make-text-block grid-left grid-top grid-width grid-height
                        cell-width cell-height fg-index bg-index))
      (else
       (unpack-error 'extension-block->text-block)))))

(define (extension-block->application-block extension-block)
  (and-let* ((data (extension-block->u8vector extension-block)))
    (bitmatch data
      (((identifier (* 8 8) bitstring)
        (auth-code (* 3 8) bitstring))
       (make-application-block (bitstring->string identifier)
                               (bitstring->string auth-code)))
      (else
       (unpack-error 'extension-block->application-block)))))

)
