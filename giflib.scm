(module giflib
  (gif? open-gif open-gif* create-gif slurp-gif spew-gif close-gif
   gif-width gif-height gif-resolution gif-bg-index gif-aspect-ratio
   gif-width-set! gif-height-set! gif-resolution-set! gif-bg-index-set! gif-aspect-ratio-set!
   gif-color-map gif-color-map-set! create-color-map color-map? color-map-resolution color-map-sorted?
   color-map-count color-map-ref color-map-set! color-map-set*! color-map-for-each color-map-for-each-indexed
   color? create-color create-color*
   color-red color-red-set! color-green color-green-set! color-blue color-blue-set!
   gif-append-extension-block! gif-extension-block-count gif-extension-block-ref gif-extension-block-for-each gif-extension-block-for-each-indexed gif-extension-blocks gif-metadata
   gif-frame-count gif-frame-ref gif-frame-for-each gif-frame-for-each-indexed
   frame? gif-append-frame! frame-width frame-width-set! frame-height frame-height-set! frame-left frame-left-set! frame-top frame-top-set! frame-interlaced? frame-interlaced?-set! frame-color-map frame-color-map-set! frame-pixel frame-pixel-set! frame-row frame-row-set! frame-rows frame-rows-set! frame-pixels frame-pixels-set!
   frame-append-extension-block! frame-extension-block-count frame-extension-block-ref frame-extension-block-for-each frame-extension-block-for-each-indexed frame-extension-blocks frame-metadata
   sub-block? make-sub-block sub-block-id sub-block-data
   comment-block? make-comment-block comment-block-text
   graphics-control-block? make-graphics-control-block graphics-control-block-disposal graphics-control-block-user-input? graphics-control-block-delay graphics-control-block-transparency-index
   text-block? make-text-block text-block-grid-left text-block-grid-top text-block-grid-width text-block-grid-height text-block-cell-width text-block-cell-height text-block-fg-index text-block-bg-index
   application-block? make-application-block application-block-identifier application-block-auth-code)

(import chicken scheme foreign)
;; TODO: make more use of srfi-1
(use extras srfi-1 srfi-4 bitstring)

#> #include "gif_lib.h" <#

;;; foreign constants

(define GIF_ERROR (foreign-value "GIF_ERROR" byte))
(define GIF_OK (foreign-value "GIF_OK" byte))

(define CONTINUE_EXT_FUNC_CODE (foreign-value "CONTINUE_EXT_FUNC_CODE" unsigned-byte))
(define COMMENT_EXT_FUNC_CODE (foreign-value "COMMENT_EXT_FUNC_CODE" unsigned-byte))
(define GRAPHICS_EXT_FUNC_CODE (foreign-value "GRAPHICS_EXT_FUNC_CODE" unsigned-byte))
(define PLAINTEXT_EXT_FUNC_CODE (foreign-value "PLAINTEXT_EXT_FUNC_CODE" unsigned-byte))
(define APPLICATION_EXT_FUNC_CODE (foreign-value "APPLICATION_EXT_FUNC_CODE" unsigned-byte))

(define DISPOSAL_UNSPECIFIED (foreign-value "DISPOSAL_UNSPECIFIED" unsigned-byte))
(define DISPOSE_DO_NOT (foreign-value "DISPOSE_DO_NOT" unsigned-byte))
(define DISPOSE_BACKGROUND (foreign-value "DISPOSE_BACKGROUND" unsigned-byte))
(define DISPOSE_PREVIOUS (foreign-value "DISPOSE_PREVIOUS" unsigned-byte))

;;; foreign functions

;; dgif_lib.c
(define DGifOpenFileName (foreign-lambda (c-pointer (struct "GifFileType")) "DGifOpenFileName" c-string (c-pointer int)))
(define DGifOpenFileHandle (foreign-lambda (c-pointer (truct "GifFileType")) "DGifOpenFileHandle" int (c-pointer int)))
(define DGifSlurp (foreign-lambda int "DGifSlurp" (c-pointer (struct "GifFileType"))))
(define DGifCloseFile (foreign-lambda int "DGifCloseFile" (c-pointer (struct "GifFileType")) (c-pointer int)))

;; egif_lib.c
(define EGifOpenFileName (foreign-lambda (c-pointer (struct "GifFileType")) "EGifOpenFileName" c-string bool (c-pointer int)))
(define EGifSpew (foreign-lambda int "EGifSpew" (c-pointer (struct "GifFileType"))))
(define EGifCloseFile (foreign-lambda int "EGifCloseFile" (c-pointer (struct "GifFileType")) (c-pointer int)))

;; gifalloc.c
(define GifMakeMapObject (foreign-lambda (c-pointer (struct "ColorMapObject")) "GifMakeMapObject" int (const (c-pointer (struct "GifColorType")))))
(define GifFreeMapObject (foreign-lambda void "GifFreeMapObject" (c-pointer (struct "ColorMapObject"))))
(define GifMakeSavedImage (foreign-lambda (c-pointer (struct "SavedImage")) "GifMakeSavedImage" (c-pointer (struct "GifFileType")) (const (c-pointer (struct "SavedImage")))))
(define GifFreeSavedImages (foreign-lambda void "GifFreeSavedImages" (c-pointer (struct "GifFileType"))))
(define GifAddExtensionBlock (foreign-lambda int "GifAddExtensionBlock" (c-pointer int) (c-pointer (c-pointer (struct "ExtensionBlock"))) int unsigned-int u8vector))

;; gif_err.c
(define GifErrorString (foreign-lambda c-string "GifErrorString" int))

;;; foreign accessors and mutators

;; GifFileType
(define GifFileType->SWidth (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->SWidth);"))
(define GifFileType->SWidth-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) (int width)) "gif->SWidth = width;"))
(define GifFileType->SHeight (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->SHeight);"))
(define GifFileType->SHeight-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) (int height)) "gif->SHeight = height;"))
(define GifFileType->SColorResolution (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->SColorResolution);"))
(define GifFileType->SColorResolution-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) (int resolution)) "gif->SColorResolution = resolution;"))
(define GifFileType->SBackGroundColor (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->SBackGroundColor);"))
(define GifFileType->SBackGroundColor-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) (int index)) "gif->SBackGroundColor = index;"))
(define GifFileType->AspectByte (foreign-lambda* unsigned-byte (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->AspectByte);"))
(define GifFileType->AspectByte-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) (unsigned-byte ratio)) "gif->AspectByte = ratio;"))
(define GifFileType->SColorMap (foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->SColorMap);"))
(define GifFileType->SColorMap-set! (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif) ((c-pointer (struct "ColorMapObject")) color_map)) "gif->SColorMap = color_map;"))
(define GifFileType->ImageCount (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->ImageCount);"))
(define GifFileType->SavedImage (foreign-lambda* (c-pointer (struct "SavedImage")) (((c-pointer (struct "GifFileType")) gif) (int i)) "C_return(&(gif->SavedImages[i]));"))
(define GifFileType->ExtensionBlockCount (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->ExtensionBlockCount);"))
(define GifFileType->ExtensionBlockCount* (foreign-lambda* (c-pointer int) (((c-pointer (struct "GifFileType")) gif)) "C_return(&(gif->ExtensionBlockCount));"))
(define GifFileType->ExtensionBlock (foreign-lambda* (c-pointer (struct "ExtensionBlock")) (((c-pointer (struct "GifFileType")) gif) (int i)) "C_return(&(gif->ExtensionBlocks[i]));"))
(define GifFileType->ExtensionBlocks* (foreign-lambda* (c-pointer (c-pointer (struct "ExtensionBlock"))) (((c-pointer (struct "GifFileType")) gif)) "C_return(&(gif->ExtensionBlocks));"))
(define GifFileType->Error (foreign-lambda* int (((c-pointer (struct "GifFileType")) gif)) "C_return(gif->Error);"))

;; ColorMapObject
(define ColorMapObject->ColorCount (foreign-lambda* int (((c-pointer (struct "ColorMapObject")) color_map)) "C_return(color_map->ColorCount);"))
(define ColorMapObject->BitsPerPixel (foreign-lambda* int (((c-pointer (struct "ColorMapObject")) color_map)) "C_return(color_map->BitsPerPixel);"))
(define ColorMapObject->SortFlag (foreign-lambda* bool (((c-pointer (struct "ColorMapObject")) color_map)) "C_return(color_map->SortFlag);"))
(define ColorMapObject->SortFlag-set! (foreign-lambda* void (((c-pointer (struct "ColorMapObject")) color_map) (bool flag)) "color_map->SortFlag = flag;"))
(define ColorMapObject->Color (foreign-lambda* (c-pointer (struct "GifColorType")) (((c-pointer (struct "ColorMapObject")) color_map) (int i)) "C_return(&(color_map->Colors[i]));"))
(define ColorMapObject->Color-set! (foreign-lambda* void (((c-pointer (struct "ColorMapObject")) color_map) (int i) ((c-pointer (struct "GifColorType")) color)) "color_map->Colors[i] = *color;"))
(define ColorMapObject->Color-set*! (foreign-lambda* void (((c-pointer (struct "ColorMapObject")) color_map) (int i) (unsigned-byte red) (unsigned-byte green) (unsigned-byte blue)) "color_map->Colors[i] = (GifColorType) { red, green, blue };"))

;; GifColorType
(define GifColorType->Red (foreign-lambda* unsigned-byte (((c-pointer (struct "GifColorType")) color)) "C_return(color->Red);"))
(define GifColorType->Red-set! (foreign-lambda* void (((c-pointer (struct "GifColorType")) color) (unsigned-byte red)) "color->Red = red;"))
(define GifColorType->Green (foreign-lambda* unsigned-byte (((c-pointer (struct "GifColorType")) color)) "C_return(color->Green);"))
(define GifColorType->Green-set! (foreign-lambda* void (((c-pointer (struct "GifColorType")) color) (unsigned-byte green)) "color->Green = green;"))
(define GifColorType->Blue (foreign-lambda* unsigned-byte (((c-pointer (struct "GifColorType")) color)) "C_return(color->Blue);"))
(define GifColorType->Blue-set! (foreign-lambda* void (((c-pointer (struct "GifColorType")) color) (unsigned-byte blue)) "color->Blue = blue;"))

;; ExtensionBlock
(define ExtensionBlock->Function (foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block)) "C_return(extension_block->Function);"))
(define ExtensionBlock->ByteCount (foreign-lambda* int (((c-pointer (struct "ExtensionBlock")) extension_block)) "C_return(extension_block->ByteCount);"))
(define ExtensionBlock->Bytes (foreign-lambda* (c-pointer unsigned-byte) (((c-pointer (struct "ExtensionBlock")) extension_block)) "C_return(extension_block->Bytes);"))
(define ExtensionBlock->u8vector (foreign-lambda* void ((u8vector dest) ((c-pointer unsigned-byte) src) (int size)) "memcpy(dest, src, size * sizeof(unsigned char));"))

;; SavedImage
(define SavedImage->Width (foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.Width);"))
(define SavedImage->Width-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (int width)) "frame->ImageDesc.Width = width;"))
(define SavedImage->Height (foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.Height);"))
(define SavedImage->Height-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (int height)) "frame->ImageDesc.Height = height;"))
(define SavedImage->Left (foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.Left);"))
(define SavedImage->Left-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (int left)) "frame->ImageDesc.Left = left;"))
(define SavedImage->Top (foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.Top);"))
(define SavedImage->Top-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (int top)) "frame->ImageDesc.Top = top;"))
(define SavedImage->Interlace (foreign-lambda* bool (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.Interlace);"))
(define SavedImage->Interlace-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (bool interlace)) "frame->ImageDesc.Interlace = interlace;"))
(define SavedImage->ColorMap (foreign-lambda* (c-pointer (struct "ColorMapObject")) (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ImageDesc.ColorMap);"))
(define SavedImage->ColorMap-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) ((c-pointer (struct "ColorMapObject")) color_map)) "frame->ImageDesc.ColorMap = color_map;"))
(define SavedImage->ExtensionBlockCount (foreign-lambda* int (((c-pointer (struct "SavedImage")) frame)) "C_return(frame->ExtensionBlockCount);"))
(define SavedImage->ExtensionBlockCount* (foreign-lambda* (c-pointer int) (((c-pointer (struct "SavedImage")) frame)) "C_return(&(frame->ExtensionBlockCount));"))
(define SavedImage->ExtensionBlock (foreign-lambda* (c-pointer (struct "ExtensionBlock")) (((c-pointer (struct "SavedImage")) frame) (int i)) "C_return(&(frame->ExtensionBlocks[i]));"))
(define SavedImage->ExtensionBlocks* (foreign-lambda* (c-pointer (c-pointer (struct "ExtensionBlock"))) (((c-pointer (struct "SavedImage")) frame)) "C_return(&(frame->ExtensionBlocks));"))
(define SavedImage->pixel (foreign-lambda* unsigned-byte (((c-pointer (struct "SavedImage")) frame) (int width) (int x) (int y)) "C_return(frame->RasterBits[y*width+x]);"))
(define SavedImage->pixel-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (int width) (int x) (int y) (unsigned-byte color)) "frame->RasterBits[y*width+x] = color;"))
(define SavedImage->row (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (u8vector dest) (int width) (int i)) "memcpy(dest, frame->RasterBits + i * width, width * sizeof(unsigned char));"))
(define SavedImage->row-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (u8vector src) (int width) (int i)) "memcpy(frame->RasterBits + i * width, src, width * sizeof(unsigned char));"))
(define SavedImage->realloc (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame)) "free(frame->RasterBits); frame->RasterBits = calloc(frame->ImageDesc.Width * frame->ImageDesc.Height, sizeof(unsigned char));"))
(define SavedImage->pixels (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (u8vector dest) (int size)) "memcpy(dest, frame->RasterBits, size * sizeof(unsigned char));"))
(define SavedImage->pixels-set! (foreign-lambda* void (((c-pointer (struct "SavedImage")) frame) (u8vector src) (int size)) "memcpy(frame->RasterBits, src, size * sizeof(unsigned char));"))

;; various

(define free-GifFileType-ExtensionBlocks (foreign-lambda* void (((c-pointer (struct "GifFileType")) gif)) "GifFreeExtensions(&(gif->ExtensionBlockCount), &(gif->ExtensionBlocks));"))

(define create-GifColorType (foreign-lambda* (c-pointer (struct "GifColorType")) () "C_return(malloc(sizeof(GifColorType)));"))
(define create-GifColorType* (foreign-lambda* (c-pointer (struct "GifColorType")) ((unsigned-byte red) (unsigned-byte green) (unsigned-byte blue)) "GifColorType *color = malloc(sizeof(GifColorType)); color->Red = red; color->Green = green; color->Blue = blue; C_return(color);"))
(define free-GifColorType (foreign-lambda* void (((c-pointer (struct "GifColorType")) color)) "free(color);"))

;;; auxiliary records

;; TODO: define record printers

(define-record gif mode pointer)
(define-record frame raster-allocated? pointer)
(define-record color-map pointer)
(define-record color pointer)
(define-record sub-block id data)
(define-record comment-block text)
(define-record graphics-control-block disposal user-input? delay transparency-index)
(define-record application-block identifier auth-code)
(define-record text-block grid-left grid-top grid-width grid-height cell-width cell-height fg-index bg-index)

;;; errors

(define (define-error location message . condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (giflib-error status location)
  (define-error location (GifErrorString status) 'giflib 'code status))

(define (oob-error index count location)
  (define-error location (format "Out of bounds: ~a / ~a" index count) 'bounds))

(define (interval-error value lower-bound upper-bound location)
  (define-error location (format "Value ~a outside interval [~a ~a]"
                                 value lower-bound upper-bound) 'range))

(define (type-error value expected location)
  (define-error location (format "Bad argument type - not a ~a: ~a"
                                 expected value) 'type))

(define (unknown-extension-block-error location)
  (define-error location "Unknown extension block" 'match))

(define (unknown-disposal-error disposal location)
  (define-error location (format "Unknown disposal ~a" disposal) 'match))

(define (unpack-error location)
  (define-error location "Unpacking error" 'match))

(define (usage-error message location)
  (define-error location message 'usage))

(define (metadata-error message location)
  (define-error location message 'metadata))

;;; setting up and tearing down gifs

(define (open-gif filename)
  (let-location ((status int 0))
    (let ((gif* (DGifOpenFileName filename (location status))))
      (if gif*
          (set-finalizer! (make-gif 'read gif*) close-gif)
          (abort (giflib-error status 'open-gif))))))

(define (open-gif* fd)
  (let-location ((status int 0))
    (let ((gif* (DGifOpenFileHandle fd (location status))))
      (if gif*
          (set-finalizer! (make-gif 'read gif*) close-gif)
          (abort (giflib-error status 'open-gif*))))))

(define (create-gif filename #!optional overwrite?)
  (let-location ((status int 0))
    (let ((gif* (EGifOpenFileName filename (not overwrite?) (location status))))
      (if gif*
          (set-finalizer! (make-gif 'write gif*) close-gif)
          (abort (giflib-error status 'create-gif))))))

(define (slurp-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    (when (= (DGifSlurp gif*) GIF_ERROR)
      (abort (giflib-error (GifFileType->Error gif*) 'slurp-gif)))))

(define (spew-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    ;; TODO: sanity checks (like frame dimensions)
    (when (= (EGifSpew gif*) GIF_ERROR)
      (abort (giflib-error (GifFileType->Error gif*) 'spew-gif)))
    ;; spewing closes the gif...
    (GifFreeSavedImages gif*)
    (free-GifFileType-ExtensionBlocks gif*)
    (gif-pointer-set! gif #f)))

(define (close-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let-location ((status int 0))
      (let ((close-fun (case (gif-mode gif)
                         ((read) DGifCloseFile)
                         ((write) EGifCloseFile))))
        (when (= (close-fun gif* (location status)) GIF_ERROR)
          (abort (giflib-error status 'close-gif)))))
    (gif-pointer-set! gif #f)))

;;; gifs

;; TODO: define SRFI-17 setters

(define (gif-width gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SWidth gif*)))

(define (gif-width-set! gif width)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SWidth-set! gif* width)))

(define (gif-height gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SHeight gif*)))

(define (gif-height-set! gif height)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SHeight-set! gif* height)))

(define (gif-resolution gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SColorResolution gif*)))

(define (gif-resolution-set! gif resolution)
  (and-let* ((gif* (gif-pointer gif)))
    (if (and (> resolution 0) (<= resolution 8))
        (GifFileType->SColorResolution-set! gif* resolution)
        (abort (interval-error resolution 1 8 'gif-resolution-set!)))))

(define (gif-bg-index gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SBackGroundColor gif*)))

(define (gif-bg-index-set! gif bg-index)
  (and-let* ((gif* (gif-pointer gif)))
    (if (and (>= bg-index 0) (< bg-index 256))
        (GifFileType->SBackGroundColor-set! gif* bg-index)
        (abort (interval-error bg-index 0 255 'gif-bg-index-set!)))))

(define epsilon 1e-6)

(define (aspect-byte->ratio aspect-byte)
  (/ (+ aspect-byte 15) 64))

(define (aspect-ratio->byte aspect-ratio)
  (inexact->exact (floor (- (* aspect-ratio 64) 15))))

(define (gif-aspect-ratio gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((aspect-byte (GifFileType->AspectByte gif*)))
      (if (zero? aspect-byte)
          #f
          (aspect-byte->ratio aspect-byte)))))

(define (gif-aspect-ratio-set! gif aspect-ratio)
  (and-let* ((gif* (gif-pointer gif)))
    (cond
     ((not aspect-ratio)
      (GifFileType->AspectByte-set! gif* 0))
     ((number? aspect-ratio)
      (let ((lower-bound (aspect-byte->ratio 0))
            (upper-bound (aspect-byte->ratio 255)))
        (if (and (or (< (abs (- lower-bound aspect-ratio)) epsilon)
                     (> aspect-ratio lower-bound))
                 (or (< (abs (- upper-bound aspect-ratio)) epsilon)
                     (< aspect-ratio upper-bound)))
            (GifFileType->AspectByte-set! gif* (aspect-ratio->byte aspect-ratio))
            (abort (interval-error aspect-ratio lower-bound upper-bound
                                   'gif-aspect-ratio-set!)))))
     (else (abort (type-error aspect-ratio "number or #f"
                              'gif-aspect-ratio-set!))))))

(define (gif-color-map gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((color-map* (GifFileType->SColorMap gif*)))
      (if color-map*
          (make-color-map color-map*)
          #f))))

(define (gif-color-map-set! gif color-map)
  (and-let* ((gif* (gif-pointer gif))
             (color-map* (color-map-pointer color-map)))
    (GifFileType->SColorMap-set! gif* color-map*)))

(define (gif-append-extension-block! gif block)
  (and-let* ((gif* (gif-pointer gif)))
    (let* ((data (specialized-block->data block))
           (length (u8vector-length data))
           (function (specialized-block->function block))
           (extension-block-count* (GifFileType->ExtensionBlockCount* gif*))
           (extension-blocks* (GifFileType->ExtensionBlocks* gif*)))
      (when (= (GifAddExtensionBlock extension-block-count* extension-blocks*
                                     function length data)
               GIF_ERROR)
        (abort (giflib-error "Failed adding extension block"
                             'gif-append-extension-block!))))))

(define (gif-extension-block-count gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->ExtensionBlockCount gif*)))

(define (gif-extension-block-ref gif index)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (if (and (>= index 0) (< index count))
          (let ((extension-block* (GifFileType->ExtensionBlock gif* index)))
            (ExtensionBlock->specialized-block extension-block*))
          (abort (oob-error index count 'gif-extension-block-ref))))))

(define (gif-extension-block-for-each proc gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (let ((extension-block* (GifFileType->ExtensionBlock gif* i)))
            (proc (ExtensionBlock->specialized-block extension-block*))
            (loop (add1 i))))))))

(define (gif-extension-block-for-each-indexed proc gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (let ((extension-block* (GifFileType->ExtensionBlock gif* i)))
            (proc (ExtensionBlock->specialized-block extension-block*) i)
            (loop (add1 i))))))))

(define (gif-extension-blocks gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0) (acc '()))
        (if (< i count)
            (let* ((extension-block* (GifFileType->ExtensionBlock gif* i))
                   (extension-block (ExtensionBlock->specialized-block extension-block*)))
              (loop (add1 i) (cons extension-block acc)))
            (reverse acc))))))

(define (gif-metadata gif)
  (let ((blocks (gif-extension-blocks gif)))
    (if (null? blocks)
        #f
        (append-map block-run->metadata (chunk-extension-blocks blocks)))))

(define (gif-frame-count gif)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->ImageCount gif*)))

(define (gif-frame-ref gif index)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (if (and (>= index 0) (< index count))
          (make-frame #t (GifFileType->SavedImage gif* index))
          (abort (oob-error index count 'gif-frame-ref))))))

(define (gif-frame-for-each proc gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (proc (make-frame #t (GifFileType->SavedImage gif* i)))
          (loop (add1 i)))))))

(define (gif-frame-for-each-indexed proc gif)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (proc (make-frame #t (GifFileType->SavedImage gif* i)) i)
          (loop (add1 i)))))))

;;; color maps

(define (create-color-map size #!optional color-map)
  (if (and (>= size 0) (<= size 255))
      (let ((color-map* (if color-map (color-map-pointer color-map) #f)))
        (set-finalizer! (make-color-map (GifMakeMapObject size color-map*)) close-color-map))
      (abort (interval-error size 0 255 'create-color-map))))

(define (close-color-map color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (GifFreeMapObject color-map*)
    (color-map-pointer-set! color-map #f)))

(define (color-map-count color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (ColorMapObject->ColorCount color-map*)))

(define (color-map-resolution color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (ColorMapObject->BitsPerPixel color-map*)))

(define (color-map-sorted? color-map)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (ColorMapObject->SortFlag color-map*)))

(define (color-map-ref color-map index)
  (and-let* ((color-map* (color-map-pointer color-map))
             (count (ColorMapObject->ColorCount color-map*)))
    (if (and (>= index 0) (< index count))
        (make-color (ColorMapObject->Color color-map* index))
        (abort (oob-error index count 'color-map-ref)))))

(define (color-map-set! color-map index color)
  (and-let* ((color-map* (color-map-pointer color-map))
             (color* (color-pointer color)))
    (ColorMapObject->Color-set! color-map* index color*)))

(define (color-map-set*! color-map index red green blue)
  (and-let* ((color-map* (color-map-pointer color-map)))
    (ColorMapObject->Color-set*! color-map* index red green blue)))

(define (color-map-for-each proc color-map)
  (and-let* ((color-map* (color-map-pointer color-map))
             (count (ColorMapObject->ColorCount color-map*)))
    (let loop ((i 0))
      (when (< i count)
        (proc (make-color (ColorMapObject->Color color-map* i)))
        (loop (add1 i))))))

(define (color-map-for-each-indexed proc color-map)
  (and-let* ((color-map* (color-map-pointer color-map))
             (count (ColorMapObject->ColorCount color-map*)))
    (let loop ((i 0))
      (when (< i count)
        (proc (make-color (ColorMapObject->Color color-map* i)) i)
        (loop (add1 i))))))

;;; colors

(define (create-color)
  (set-finalizer! (make-color (create-GifColorType)) close-color))

(define (create-color* red green blue)
  (set-finalizer! (make-color (create-GifColorType* red green blue)) close-color))

(define (close-color color)
  (and-let* ((color* (color-pointer color)))
    (free-GifColorType color*)
    (color-pointer-set! color #f)))

(define (color-red color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Red color*)))

(define (color-red-set! color red)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Red-set! color* red)))

(define (color-green color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Green color*)))

(define (color-green-set! color green)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Green-set! color* green)))

(define (color-blue color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Blue color*)))

(define (color-blue-set! color blue)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Blue-set! color* blue)))

;;; frames

;; frames are special as they only make sense in the context of a gif,
;; so you need to specify a gif when appending a new one and can only
;; free all of them at once
(define (gif-append-frame! gif #!optional frame)
  (and-let* ((gif* (gif-pointer gif)))
    (make-frame #f (GifMakeSavedImage gif* (if frame (frame-pointer frame) #f)))))

(define (frame-width frame)
  (SavedImage->Width (frame-pointer frame)))

(define (frame-width-set! frame width)
  (let* ((frame* (frame-pointer frame))
         (old-width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*)))
    (if (positive? width)
        (begin
          (SavedImage->Width-set! frame* width)
          (when (and (not (= old-width width)) (positive? height))
            (SavedImage->realloc frame*)
            (frame-raster-allocated?-set! frame #t)))
        (abort (type-error width "positive width" 'frame-width-set!)))))

(define (frame-height frame)
  (SavedImage->Height (frame-pointer frame)))

(define (frame-height-set! frame height)
  (let* ((frame* (frame-pointer frame))
         (old-height (SavedImage->Height frame*))
         (width (SavedImage->Width frame*)))
    (if (positive? height)
        (begin
          (SavedImage->Height-set! frame* height)
          (when (and (not (= old-height height)) (positive? width))
            (SavedImage->realloc frame*)
            (frame-raster-allocated?-set! frame #t)))
        (abort (type-error height "positive height" 'frame-height-set!)))))

(define (frame-left frame)
  (SavedImage->Left (frame-pointer frame)))

(define (frame-left-set! frame left)
  (if (not (negative? left))
      (SavedImage->Left-set! (frame-pointer frame) left)
      (abort (type-error left "non-negative left" 'frame-left-set!))))

(define (frame-top frame)
  (SavedImage->Top (frame-pointer frame)))

(define (frame-top-set! frame top)
  (if (not (negative? top))
      (SavedImage->Top-set! (frame-pointer frame) top)
      (abort (type-error top "non-negative top" 'frame-top-set!))))

(define (frame-interlaced? frame)
  (SavedImage->Interlace (frame-pointer frame)))

(define (frame-interlaced?-set! frame interlaced?)
  (SavedImage->Interlace-set! (frame-pointer frame) interlaced?))

(define (frame-color-map frame)
  (let ((color-map* (SavedImage->ColorMap (frame-pointer frame))))
    (if color-map*
        (make-color-map color-map*)
        #f)))

(define (frame-color-map-set! frame color-map)
  (and-let* ((frame* (frame-pointer frame))
             (color-map* (color-map-pointer color-map)))
    (SavedImage->ColorMap-set! frame* color-map*)))

(define (frame-pixel frame x y)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*)))
    (if (and (>= x 0) (>= y 0)
             (< x width) (< y height))
        (SavedImage->pixel frame* width x y)
        (abort (oob-error (format "~a|~a" x y) (format "~ax~a" width height)
                          'frame-pixel)))))

(define (frame-pixel-set! frame x y index)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*)))
    (when (not (frame-raster-allocated? frame))
      (abort (usage-error "Set width and height first" 'frame-pixel-set!)))
    (if (and (>= x 0) (>= y 0)
             (< x width) (< y height))
        (SavedImage->pixel-set! frame* width x y index)
        (abort (oob-error (format "~a|~a" x y) (format "~ax~a" width height)
                          'frame-pixel-set!)))))

(define (frame-row frame index)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*))
         (row (make-u8vector width 0)))
    (if (and (>= index 0) (< index height))
        (begin
          (SavedImage->row frame* row width index)
          row)
        (abort (oob-error index height 'frame-row)))))

(define (frame-row-set! frame index row)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*))
         (row-length (u8vector-length row)))
    (when (not (frame-raster-allocated? frame))
      (abort (usage-error "Set width and height first" 'frame-row-set!)))
    (when (not (and (>= index 0) (< index height)))
      (abort (oob-error index height 'frame-row-set!)))
    (when (not (= row-length width))
      (abort (type-error row-length "correct length" 'frame-row-set!)))
    (SavedImage->row-set! frame* row width index)))

(define (frame-rows frame)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*))
         (data (make-vector height #f)))
    (let loop ((i 0))
      (when (< i height)
        (let ((row (make-u8vector width 0)))
          (SavedImage->row frame* row width i)
          (vector-set! data i row)
          (loop (add1 i)))))
    data))

(define (frame-rows-set! frame rows)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*)))
    (when (not (frame-raster-allocated? frame))
      (abort (usage-error "Set width and height first" 'frame-rows-set!)))
    (when (not (= (vector-length rows) height))
      (abort (type-error (vector-length rows) "correct length" 'frame-rows-set!)))
    (let loop ((i 0))
      (when (< i height)
        (let* ((row (vector-ref rows i))
               (row-length (u8vector-length row)))
          (when (not (= row-length width))
            (abort (type-error row-length "correct width" 'frame-rows-set!)))
          (SavedImage->row-set! frame* row width i)
          (loop (add1 i)))))))

(define (frame-pixels frame)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*))
         (size (* width height))
         (data (make-u8vector size)))
    (SavedImage->pixels frame* data size)
    data))

(define (frame-pixels-set! frame pixels)
  (let* ((frame* (frame-pointer frame))
         (width (SavedImage->Width frame*))
         (height (SavedImage->Height frame*))
         (size (* width height))
         (pixels-length (u8vector-length pixels)))
    (when (not (frame-raster-allocated? frame))
      (abort (usage-error "Set width and height first" 'frame-pixels-set!)))
    (when (not (= pixels-length size))
      (abort (type-error size "correct length" 'frame-pixels-set!)))
    (SavedImage->pixels-set! frame* pixels size)))

(define (frame-append-extension-block! frame block)
  (let* ((data (specialized-block->data block))
         (length (u8vector-length data))
         (function (specialized-block->function block))
         (frame* (frame-pointer frame))
         (extension-block-count* (SavedImage->ExtensionBlockCount* frame*))
         (extension-blocks* (SavedImage->ExtensionBlocks* frame*)))
    (when (= (GifAddExtensionBlock extension-block-count* extension-blocks*
                                   function length data)
             GIF_ERROR)
      (abort (giflib-error "Failed adding extension block"
                           'frame-append-extension-block!)))))

(define (frame-extension-block-count frame)
  (SavedImage->ExtensionBlockCount (frame-pointer frame)))

(define (frame-extension-block-ref frame index)
  (let* ((frame* (frame-pointer frame))
         (count (SavedImage->ExtensionBlockCount frame*)))
    (if (and (>= index 0) (< index count))
        (let ((extension-block* (SavedImage->ExtensionBlock frame* index)))
          (ExtensionBlock->specialized-block extension-block*))
        (abort (oob-error index count 'frame-extension-block-ref)))))

(define (frame-extension-block-for-each proc frame)
  (let* ((frame* (frame-pointer frame))
         (count (SavedImage->ExtensionBlockCount frame*)))
    (let loop ((i 0))
      (when (< i count)
        (let ((extension-block* (SavedImage->ExtensionBlock frame* i)))
          (proc (ExtensionBlock->specialized-block extension-block*))
          (loop (add1 i)))))))

(define (frame-extension-block-for-each-indexed proc frame)
  (let* ((frame* (frame-pointer frame))
         (count (SavedImage->ExtensionBlockCount frame*)))
    (let loop ((i 0))
      (when (< i count)
        (let ((extension-block* (SavedImage->ExtensionBlock frame* i)))
          (proc (ExtensionBlock->specialized-block extension-block*) i)
          (loop (add1 i)))))))

(define (frame-extension-blocks frame)
  (let* ((frame* (frame-pointer frame))
         (count (SavedImage->ExtensionBlockCount frame*)))
    (let loop ((i 0) (acc '()))
      (if (< i count)
          (let* ((extension-block* (SavedImage->ExtensionBlock frame* i))
                 (extension-block (ExtensionBlock->specialized-block extension-block*)))
            (loop (add1 i) (cons extension-block acc)))
          (reverse acc)))))

(define (frame-metadata gif)
  (let ((blocks (frame-extension-blocks gif)))
    (if (null? blocks)
        #f
        (append-map block-run->metadata (chunk-extension-blocks blocks)))))

;;; extension blocks

;; packing and unpacking

(bitpacket sub-block
  (id 8 little)
  (data bitstring))

(define (data->sub-block data)
  (bitmatch data
    (((sub-block bitpacket))
     (make-sub-block id (bitstring->u8vector data)))
    (else (abort (unpack-error 'data->sub-block)))))

(define (sub-block->data block)
  (let ((id (sub-block-id block))
        (data (sub-block-data block)))
    (bitstring->u8vector (bitconstruct (sub-block bitpacket)))))

(define (data->comment-block data)
  (make-comment-block (blob->string (u8vector->blob data))))

(define (comment-block->data block)
  (blob->u8vector (string->blob (comment-block-text block))))

(bitpacket graphics-control-block
  (reserved 3 little)
  (disposal 3 little)
  (user-input? 1 boolean little)
  (transparency-index? 1 boolean little)
  (delay-time (* 2 8) little)
  (transparency-index 8 little))

(define (data->graphics-control-block data)
  (bitmatch data
    (((graphics-control-block bitpacket))
     (make-graphics-control-block
      (select disposal
        ((DISPOSAL_UNSPECIFIED) 'unspecified)
        ((DISPOSE_DO_NOT) 'none)
        ((DISPOSE_BACKGROUND) 'background)
        ((DISPOSE_PREVIOUS) 'previous)
        (else (abort (unknown-disposal-error disposal 'data->graphics-control-block))))
      user-input? delay-time (and transparency-index? transparency-index)))
    (else (abort (unpack-error 'data->graphics-control-block)))))

(define (graphics-control-block->data block)
  (let ((reserved 0)
        (disposal
         (case (graphics-control-block-disposal block)
           ((unspecified) DISPOSAL_UNSPECIFIED)
           ((none) DISPOSE_DO_NOT)
           ((background) DISPOSE_BACKGROUND)
           ((previous) DISPOSE_PREVIOUS)
           (else (abort (unknown-disposal-error block 'graphics-control-block->data)))))
        (user-input? (graphics-control-block-user-input? block))
        (transparency-index? (and (graphics-control-block-transparency-index block) #t))
        (delay-time (graphics-control-block-delay block))
        (transparency-index (or (graphics-control-block-transparency-index block) 0)))
    (bitstring->u8vector (bitconstruct (graphics-control-block bitpacket)))))

(bitpacket text-block
  (grid-left (* 2 8) little)
  (grid-top (* 2 8) little)
  (grid-width (* 2 8) little)
  (grid-height (* 2 8) little)
  (cell-width 8 little)
  (cell-height 8 little)
  (fg-index 8 little)
  (bg-index 8 little))

(define (data->text-block data)
  (bitmatch data
    (((text-block bitpacket))
     (make-text-block grid-left grid-top grid-width grid-height
                      cell-width cell-height fg-index bg-index))
    (else (abort (unpack-error 'data->text-block)))))

(define (text-block->data block)
  (let ((grid-left (text-block-grid-left block))
        (grid-top (text-block-grid-top block))
        (grid-width (text-block-grid-width block))
        (grid-height (text-block-grid-height block))
        (cell-width (text-block-cell-width block))
        (cell-height (text-block-cell-height block))
        (fg-index (text-block-fg-index block))
        (bg-index (text-block-bg-index block)))
    (bitstring->u8vector (bitconstruct (text-block bitpacket)))))

(bitpacket application-block
  (identifier (* 8 8) bitstring)
  (auth-code (* 3 8) bitstring))

(define (data->application-block data)
  (bitmatch data
    (((application-block bitpacket))
     (make-application-block (bitstring->string identifier)
                             (bitstring->string auth-code)))
    (else (abort (unpack-error 'data->application-block)))))

(define (application-block->data block)
  (let ((identifier (application-block-identifier block))
        (auth-code (application-block-auth-code block)))
    (bitstring->u8vector (bitconstruct (application-block bitpacket)))))

;; specialization and unspecialization

(define (ExtensionBlock->specialized-block extension-block*)
  (let* ((function (ExtensionBlock->Function extension-block*))
         (data-length (ExtensionBlock->ByteCount extension-block*))
         (data-pointer (ExtensionBlock->Bytes extension-block*))
         (data (make-u8vector data-length 0)))
    (ExtensionBlock->u8vector data data-pointer data-length)
    (select function
      ((CONTINUE_EXT_FUNC_CODE) (data->sub-block data))
      ((COMMENT_EXT_FUNC_CODE) (data->comment-block data))
      ((GRAPHICS_EXT_FUNC_CODE) (data->graphics-control-block data))
      ((PLAINTEXT_EXT_FUNC_CODE) (data->text-block data))
      ((APPLICATION_EXT_FUNC_CODE) (data->application-block data))
      (else (abort (unknown-extension-block-error 'ExtensionBlock->specialized-block))))))

(define (specialized-block->data block)
  (cond
   ((sub-block? block) (sub-block->data block))
   ((comment-block? block) (comment-block->data block))
   ((graphics-control-block? block) (graphics-control-block->data block))
   ((text-block? block) (text-block->data block))
   ((application-block? block) (application-block->data block))
   (else (abort (unknown-extension-block-error 'specialized-block->data)))))

(define (specialized-block->function block)
  (cond
   ((sub-block? block) CONTINUE_EXT_FUNC_CODE)
   ((comment-block? block) COMMENT_EXT_FUNC_CODE)
   ((graphics-control-block? block) GRAPHICS_EXT_FUNC_CODE)
   ((text-block? block) PLAINTEXT_EXT_FUNC_CODE)
   ((application-block? block) APPLICATION_EXT_FUNC_CODE)
   (else (abort (unknown-extension-block-error 'specialized-block->data)))))

;; extension block metadata

(define (sub-block->loop-count block)
  (let* ((data (sub-block-data block))
         ;; NOTE: data is stored LE
         (count (+ (* (u8vector-ref data 1) 256) (u8vector-ref data 0))))
    (if (zero? count)
        #t
        count)))

(define (chunk-extension-blocks blocks)
  (let loop ((blocks blocks) (acc '()) (chunk '()))
    (if (not (null? blocks))
        (let ((block (car blocks)))
          (if (sub-block? block)
              (loop (cdr blocks) acc (append chunk (list block)))
              (if (null? chunk)
                  (loop (cdr blocks) acc (list block))
                  (loop blocks (append acc (list chunk)) '()))))
        (append acc (list chunk)))))

(define (block-run->metadata block-run)
  (cond
   ((= (length block-run) 1)
    (let ((block (car block-run)))
      (cond
       ;; NOTE: comment blocks should be followed by sub blocks
       ;; containing the text, but they are not in giflib...
       ((comment-block? block)
        `((comment . ,(comment-block-text block))))
       ((graphics-control-block? block)
        `((disposal . ,(graphics-control-block-disposal block))
          (user-input? . ,(graphics-control-block-user-input? block))
          (delay . ,(graphics-control-block-delay block))
          (transparency-index . ,(graphics-control-block-transparency-index block))))
       (else (abort (metadata-error "Extension block not followed by mandatory sub blocks"
                                    'block-run->metadata))))))
   ((> (length block-run) 1)
    (let ((start (car block-run))
          (sub-blocks (cdr block-run)))
      (cond
       ((application-block? start)
        (let ((identifier (application-block-identifier start))
              (auth-code (application-block-auth-code start))
              (data (map sub-block-data sub-blocks)))
          (if (and (equal? identifier "NETSCAPE")
                   (equal? auth-code "2.0")
                   (= (length sub-blocks) 1))
              `((loop . ,(sub-block->loop-count (car sub-blocks))))
              `((application
                 (identifier . ,identifier)
                 (auth-code . ,auth-code)
                 (data . ,(map sub-block-data sub-blocks)))))))
       ;; NOTE: untested due to the lack of gifs using this feature
       ((text-block? start)
        `((text
           (grid-left . ,(text-block-grid-left start))
           (grid-top . ,(text-block-grid-top start))
           (grid-width . ,(text-block-grid-width start))
           (grid-height . ,(text-block-grid-height start))
           (cell-width . ,(text-block-cell-width start))
           (cell-height . ,(text-block-cell-height start))
           (fg-index . ,(text-block-fg-index start))
           (bg-index . ,(text-block-bg-index start))
           (data . ,(map sub-block-data sub-blocks)))))
       (else (abort (metadata-error "Self-contained extension block followed by sub blocks"
                                    'block-run->metadata))))))
   (else (abort (metadata-error "Invalid extension block run" 'block-run->metadata)))))

;;; TODO: imlib2 interface

;; (define (gif->iblib2-frames gif))
;; (define (gif-imlib2-frame-for-each proc gif))
;; (define (gif-imlib2-frame-for-each-indexed proc gif))

)
