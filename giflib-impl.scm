#> #include "gif_lib.h" <#

;; typedefs
(define-foreign-type GifFileType* (nonnull-c-pointer (struct "GifFileType")))
(define-foreign-type nullable-GifFileType* (c-pointer (struct "GifFileType")))
(define-foreign-type ColorMapObject* (nonnull-c-pointer (struct "ColorMapObject")))
(define-foreign-type nullable-ColorMapObject* (c-pointer (struct "ColorMapObject")))
(define-foreign-type GifColorType* (nonnull-c-pointer (struct "GifColorType")))
(define-foreign-type nullable-GifColorType* (c-pointer (struct "GifColorType")))
(define-foreign-type SavedImage* (nonnull-c-pointer (struct "SavedImage")))
(define-foreign-type nullable-SavedImage* (c-pointer (struct "SavedImage")))
(define-foreign-type ExtensionBlock* (nonnull-c-pointer (struct "ExtensionBlock")))
(define-foreign-type ExtensionBlock** (nonnull-c-pointer (nonnull-c-pointer (struct "ExtensionBlock"))))

(define-foreign-type int* (nonnull-c-pointer int))
(define-foreign-type uint8* (nonnull-c-pointer unsigned-byte))
(define-foreign-type uint8 unsigned-byte)

;;; foreign constants

(define GIF_ERROR (foreign-value "GIF_ERROR" byte))
(define GIF_OK (foreign-value "GIF_OK" byte))

(define CONTINUE_EXT_FUNC_CODE (foreign-value "CONTINUE_EXT_FUNC_CODE" uint8))
(define COMMENT_EXT_FUNC_CODE (foreign-value "COMMENT_EXT_FUNC_CODE" uint8))
(define GRAPHICS_EXT_FUNC_CODE (foreign-value "GRAPHICS_EXT_FUNC_CODE" uint8))
(define PLAINTEXT_EXT_FUNC_CODE (foreign-value "PLAINTEXT_EXT_FUNC_CODE" uint8))
(define APPLICATION_EXT_FUNC_CODE (foreign-value "APPLICATION_EXT_FUNC_CODE" uint8))

(define DISPOSAL_UNSPECIFIED (foreign-value "DISPOSAL_UNSPECIFIED" uint8))
(define DISPOSE_DO_NOT (foreign-value "DISPOSE_DO_NOT" uint8))
(define DISPOSE_BACKGROUND (foreign-value "DISPOSE_BACKGROUND" uint8))
(define DISPOSE_PREVIOUS (foreign-value "DISPOSE_PREVIOUS" uint8))

;;; foreign functions

;; dgif_lib.c
(define DGifOpenFileName (foreign-lambda nullable-GifFileType* "DGifOpenFileName" nonnull-c-string int*))
(define DGifSlurp (foreign-lambda int "DGifSlurp" GifFileType*))
(define DGifCloseFile (foreign-lambda int "DGifCloseFile" GifFileType* int*))

;; egif_lib.c
(define EGifOpenFileName (foreign-lambda nullable-GifFileType* "EGifOpenFileName" nonnull-c-string bool int*))
(define EGifSpew (foreign-lambda int "EGifSpew" GifFileType*))
(define EGifCloseFile (foreign-lambda int "EGifCloseFile" GifFileType* int*))

;; gifalloc.c
(define GifMakeMapObject (foreign-lambda ColorMapObject* "GifMakeMapObject" int (const nullable-GifColorType*)))
(define GifFreeMapObject (foreign-lambda void "GifFreeMapObject" ColorMapObject*))
(define GifMakeSavedImage (foreign-lambda SavedImage* "GifMakeSavedImage" GifFileType* (const nullable-SavedImage*)))
(define GifFreeSavedImages (foreign-lambda void "GifFreeSavedImages" GifFileType*))
(define GifAddExtensionBlock (foreign-lambda int "GifAddExtensionBlock" int* ExtensionBlock** int unsigned-int u8vector))

;; gif_err.c
(define GifErrorString (foreign-lambda c-string "GifErrorString" int))

;;; foreign accessors and mutators

;; GifFileType
(define GifFileType->SWidth (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->SWidth);"))
(define GifFileType->SWidth-set! (foreign-lambda* void ((GifFileType* gif) (int width)) "gif->SWidth = width;"))
(define GifFileType->SHeight (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->SHeight);"))
(define GifFileType->SHeight-set! (foreign-lambda* void ((GifFileType* gif) (int height)) "gif->SHeight = height;"))
(define GifFileType->SColorResolution (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->SColorResolution);"))
(define GifFileType->SColorResolution-set! (foreign-lambda* void ((GifFileType* gif) (int resolution)) "gif->SColorResolution = resolution;"))
(define GifFileType->SBackGroundColor (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->SBackGroundColor);"))
(define GifFileType->SBackGroundColor-set! (foreign-lambda* void ((GifFileType* gif) (int index)) "gif->SBackGroundColor = index;"))
(define GifFileType->AspectByte (foreign-lambda* uint8 ((GifFileType* gif)) "C_return(gif->AspectByte);"))
(define GifFileType->AspectByte-set! (foreign-lambda* void ((GifFileType* gif) (uint8 ratio)) "gif->AspectByte = ratio;"))
(define GifFileType->SColorMap (foreign-lambda* nullable-ColorMapObject* ((GifFileType* gif)) "C_return(gif->SColorMap);"))
(define GifFileType->SColorMap-set! (foreign-lambda* void ((GifFileType* gif) (ColorMapObject* color_map)) "gif->SColorMap = color_map;"))
(define GifFileType->ImageCount (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->ImageCount);"))
(define GifFileType->SavedImage (foreign-lambda* SavedImage* ((GifFileType* gif) (int i)) "C_return(&(gif->SavedImages[i]));"))
(define GifFileType->ExtensionBlockCount (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->ExtensionBlockCount);"))
(define GifFileType->ExtensionBlockCount* (foreign-lambda* int* ((GifFileType* gif)) "C_return(&(gif->ExtensionBlockCount));"))
(define GifFileType->ExtensionBlock (foreign-lambda* ExtensionBlock* ((GifFileType* gif) (int i)) "C_return(&(gif->ExtensionBlocks[i]));"))
(define GifFileType->ExtensionBlocks* (foreign-lambda* ExtensionBlock** ((GifFileType* gif)) "C_return(&(gif->ExtensionBlocks));"))
(define GifFileType->Error (foreign-lambda* int ((GifFileType* gif)) "C_return(gif->Error);"))

;; ColorMapObject
(define ColorMapObject->ColorCount (foreign-lambda* int ((ColorMapObject* color_map)) "C_return(color_map->ColorCount);"))
(define ColorMapObject->BitsPerPixel (foreign-lambda* int ((ColorMapObject* color_map)) "C_return(color_map->BitsPerPixel);"))
(define ColorMapObject->SortFlag (foreign-lambda* bool ((ColorMapObject* color_map)) "C_return(color_map->SortFlag);"))
(define ColorMapObject->SortFlag-set! (foreign-lambda* void ((ColorMapObject* color_map) (bool flag)) "color_map->SortFlag = flag;"))
(define ColorMapObject->Colors (foreign-lambda* GifColorType* ((ColorMapObject* color_map)) "color_map->Colors;"))
(define ColorMapObject->Color (foreign-lambda* GifColorType* ((ColorMapObject* color_map) (int i)) "C_return(&(color_map->Colors[i]));"))
(define ColorMapObject->Color-set! (foreign-lambda* void ((ColorMapObject* color_map) (int i) (GifColorType* color)) "color_map->Colors[i] = *color;"))
(define ColorMapObject->Color-set*! (foreign-lambda* void ((ColorMapObject* color_map) (int i) (uint8 red) (uint8 green) (uint8 blue)) "color_map->Colors[i] = (GifColorType) { red, green, blue };"))

;; GifColorType
(define GifColorType->Red (foreign-lambda* uint8 ((GifColorType* color)) "C_return(color->Red);"))
(define GifColorType->Red-set! (foreign-lambda* void ((GifColorType* color) (uint8 red)) "color->Red = red;"))
(define GifColorType->Green (foreign-lambda* uint8 ((GifColorType* color)) "C_return(color->Green);"))
(define GifColorType->Green-set! (foreign-lambda* void ((GifColorType* color) (uint8 green)) "color->Green = green;"))
(define GifColorType->Blue (foreign-lambda* uint8 ((GifColorType* color)) "C_return(color->Blue);"))
(define GifColorType->Blue-set! (foreign-lambda* void ((GifColorType* color) (uint8 blue)) "color->Blue = blue;"))

;; ExtensionBlock
(define ExtensionBlock->Function (foreign-lambda* int ((ExtensionBlock* extension_block)) "C_return(extension_block->Function);"))
(define ExtensionBlock->ByteCount (foreign-lambda* int ((ExtensionBlock* extension_block)) "C_return(extension_block->ByteCount);"))
(define ExtensionBlock->Bytes (foreign-lambda* uint8* ((ExtensionBlock* extension_block)) "C_return(extension_block->Bytes);"))
(define ExtensionBlock->u8vector (foreign-lambda* void ((u8vector dest) (uint8* src) (int size)) "memcpy(dest, src, size * sizeof(unsigned char));"))

;; SavedImage
(define SavedImage->Width (foreign-lambda* int ((SavedImage* frame)) "C_return(frame->ImageDesc.Width);"))
(define SavedImage->Width-set! (foreign-lambda* void ((SavedImage* frame) (int width)) "frame->ImageDesc.Width = width;"))
(define SavedImage->Height (foreign-lambda* int ((SavedImage* frame)) "C_return(frame->ImageDesc.Height);"))
(define SavedImage->Height-set! (foreign-lambda* void ((SavedImage* frame) (int height)) "frame->ImageDesc.Height = height;"))
(define SavedImage->Left (foreign-lambda* int ((SavedImage* frame)) "C_return(frame->ImageDesc.Left);"))
(define SavedImage->Left-set! (foreign-lambda* void ((SavedImage* frame) (int left)) "frame->ImageDesc.Left = left;"))
(define SavedImage->Top (foreign-lambda* int ((SavedImage* frame)) "C_return(frame->ImageDesc.Top);"))
(define SavedImage->Top-set! (foreign-lambda* void ((SavedImage* frame) (int top)) "frame->ImageDesc.Top = top;"))
(define SavedImage->Interlace (foreign-lambda* bool ((SavedImage* frame)) "C_return(frame->ImageDesc.Interlace);"))
(define SavedImage->Interlace-set! (foreign-lambda* void ((SavedImage* frame) (bool interlace)) "frame->ImageDesc.Interlace = interlace;"))
(define SavedImage->ColorMap (foreign-lambda* nullable-ColorMapObject* ((SavedImage* frame)) "C_return(frame->ImageDesc.ColorMap);"))
(define SavedImage->ColorMap-set! (foreign-lambda* void ((SavedImage* frame) (ColorMapObject* color_map)) "frame->ImageDesc.ColorMap = color_map;"))
(define SavedImage->ExtensionBlockCount (foreign-lambda* int ((SavedImage* frame)) "C_return(frame->ExtensionBlockCount);"))
(define SavedImage->ExtensionBlockCount* (foreign-lambda* int* ((SavedImage* frame)) "C_return(&(frame->ExtensionBlockCount));"))
(define SavedImage->ExtensionBlock (foreign-lambda* ExtensionBlock* ((SavedImage* frame) (int i)) "C_return(&(frame->ExtensionBlocks[i]));"))
(define SavedImage->ExtensionBlocks* (foreign-lambda* ExtensionBlock** ((SavedImage* frame)) "C_return(&(frame->ExtensionBlocks));"))
(define SavedImage->pixel (foreign-lambda* uint8 ((SavedImage* frame) (int width) (int x) (int y)) "C_return(frame->RasterBits[y*width+x]);"))
(define SavedImage->pixel-set! (foreign-lambda* void ((SavedImage* frame) (int width) (int x) (int y) (uint8 color)) "frame->RasterBits[y*width+x] = color;"))
(define SavedImage->row (foreign-lambda* void ((SavedImage* frame) (u8vector dest) (int width) (int i)) "memcpy(dest, frame->RasterBits + i * width, width * sizeof(unsigned char));"))
(define SavedImage->row-set! (foreign-lambda* void ((SavedImage* frame) (u8vector src) (int width) (int i)) "memcpy(frame->RasterBits + i * width, src, width * sizeof(unsigned char));"))
(define SavedImage->realloc (foreign-lambda* void ((SavedImage* frame)) "free(frame->RasterBits); frame->RasterBits = calloc(frame->ImageDesc.Width * frame->ImageDesc.Height, sizeof(unsigned char));"))
(define SavedImage->pixels (foreign-lambda* void ((SavedImage* frame) (u8vector dest) (int size)) "memcpy(dest, frame->RasterBits, size * sizeof(unsigned char));"))
(define SavedImage->pixels-set! (foreign-lambda* void ((SavedImage* frame) (u8vector src) (int size)) "memcpy(frame->RasterBits, src, size * sizeof(unsigned char));"))

;; various

(define free-GifFileType-ExtensionBlocks (foreign-lambda* void ((GifFileType* gif)) "GifFreeExtensions(&(gif->ExtensionBlockCount), &(gif->ExtensionBlocks));"))

;;; auxiliary records

(define (format-pointer pointer)
  (if pointer
      (sprintf "0x~x" (pointer->address pointer))
      "NULL"))

(define-record gif mode slurped? pointer)
(define-record-printer (gif g out)
  (if (or (and (eq? (gif-mode g) 'read)
               (not (gif-slurped? g)))
          (not (gif-width g))
          (not (gif-height g)))
      (fprintf out "#<gif unknown size ~a>"
               (format-pointer (gif-pointer g)))
      (fprintf out "#<gif ~ax~a ~af ~a>"
               (gif-width g) (gif-height g)
               (gif-frame-count g)
               (format-pointer (gif-pointer g)))))

(define-record frame raster-allocated? pointer)
(define-record-printer (frame f out)
  (fprintf out "#<frame ~a|~a ~ax~a ~a>"
           (frame-left f) (frame-top f)
           (frame-width f) (frame-height f)
           (format-pointer (frame-pointer f))))

(define-record color-map pointer)
(define-record-printer (color-map c out)
  (fprintf out "#<color-map ~ac ~a>"
           (color-map-count c)
           (format-pointer (color-map-pointer c))))

(define-record color pointer)
(define-record-printer (color c out)
  (fprintf out "#<color ~a|~a|~a ~a>"
           (color-red c) (color-green c) (color-blue c)
           (format-pointer (color-pointer c))))

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

(define disposal-strategy (make-parameter #f))

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
          (set-finalizer! (make-gif 'read #f gif*) close-gif)
          (abort (giflib-error status 'open-gif))))))

(define (create-gif filename #!optional overwrite?)
  (let-location ((status int 0))
    (let ((gif* (EGifOpenFileName filename (not overwrite?) (location status))))
      (if gif*
          (set-finalizer! (make-gif 'write #f gif*) close-gif)
          (abort (giflib-error status 'create-gif))))))

(define (slurp-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    (if (= (DGifSlurp gif*) GIF_ERROR)
        (abort (giflib-error (GifFileType->Error gif*) 'slurp-gif))
        (gif-slurped?-set! gif #t))))

;; TODO: test writing gifs
;; - create gif, set params, write out
;; - create gif, copy other gif's params, write out

;; TODO: port giflib tests (turning data into gif into data, compare)

(define (spew-gif gif)
  (and-let* ((gif* (gif-pointer gif)))
    ;; TODO: sanity checks (like frame dimensions as these cannot be
    ;; completely checked when setting)
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

(define (assert-gif-slurped! gif location)
  (when (and (eq? (gif-mode gif) 'read)
             (not (gif-slurped? gif)))
    (abort (usage-error "Gif not slurped yet" location))))

(define (gif-width gif)
  (assert-gif-slurped! gif 'gif-width)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SWidth gif*)))

(define (gif-width-set! gif width)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SWidth-set! gif* width)))

(define gif-width (getter-with-setter gif-width gif-width-set!))

(define (gif-height gif)
  (assert-gif-slurped! gif 'gif-height)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SHeight gif*)))

(define (gif-height-set! gif height)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SHeight-set! gif* height)))

(define gif-height (getter-with-setter gif-height gif-height-set!))

(define (gif-resolution gif)
  (assert-gif-slurped! gif 'gif-resolution)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SColorResolution gif*)))

(define (gif-resolution-set! gif resolution)
  (and-let* ((gif* (gif-pointer gif)))
    (if (and (> resolution 0) (<= resolution 8))
        (GifFileType->SColorResolution-set! gif* resolution)
        (abort (interval-error resolution 1 8 'gif-resolution-set!)))))

(define gif-resolution (getter-with-setter gif-resolution gif-resolution-set!))

(define (gif-bg-index gif)
  (assert-gif-slurped! gif 'gif-bg-index)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->SBackGroundColor gif*)))

(define (gif-bg-index-set! gif bg-index)
  (and-let* ((gif* (gif-pointer gif)))
    (if (and (>= bg-index 0) (< bg-index 256))
        (GifFileType->SBackGroundColor-set! gif* bg-index)
        (abort (interval-error bg-index 0 255 'gif-bg-index-set!)))))

(define gif-bg-index (getter-with-setter gif-bg-index gif-bg-index-set!))

(define epsilon 1e-6)

(define (aspect-byte->ratio aspect-byte)
  (/ (+ aspect-byte 15) 64))

(define (aspect-ratio->byte aspect-ratio)
  (inexact->exact (floor (- (* aspect-ratio 64) 15))))

(define (gif-aspect-ratio gif)
  (assert-gif-slurped! gif 'gif-aspect-ratio)
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

(define gif-aspect-ratio (getter-with-setter gif-aspect-ratio gif-aspect-ratio-set!))

(define (gif-color-map gif)
  (assert-gif-slurped! gif 'gif-color-map)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((color-map* (GifFileType->SColorMap gif*)))
      (if color-map*
          (make-color-map color-map*)
          #f))))

(define (gif-color-map-set! gif color-map)
  (and-let* ((gif* (gif-pointer gif))
             (color-map* (color-map-pointer color-map)))
    (GifFileType->SColorMap-set! gif* color-map*)))

(define gif-color-map (getter-with-setter gif-color-map gif-color-map-set!))

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
  (assert-gif-slurped! gif 'gif-extension-block-count)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->ExtensionBlockCount gif*)))

(define (gif-extension-block-ref gif index)
  (assert-gif-slurped! gif 'gif-extension-block-ref)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (if (and (>= index 0) (< index count))
          (let ((extension-block* (GifFileType->ExtensionBlock gif* index)))
            (ExtensionBlock->specialized-block extension-block*))
          (abort (oob-error index count 'gif-extension-block-ref))))))

(define (gif-extension-block-for-each proc gif)
  (assert-gif-slurped! gif 'gif-extension-block-for-each)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (let ((extension-block* (GifFileType->ExtensionBlock gif* i)))
            (proc (ExtensionBlock->specialized-block extension-block*))
            (loop (add1 i))))))))

(define (gif-extension-block-for-each-indexed proc gif)
  (assert-gif-slurped! gif 'gif-extension-block-for-each-indexed)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (let ((extension-block* (GifFileType->ExtensionBlock gif* i)))
            (proc (ExtensionBlock->specialized-block extension-block*) i)
            (loop (add1 i))))))))

(define (gif-extension-blocks gif)
  (assert-gif-slurped! gif 'gif-extension-blocks)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ExtensionBlockCount gif*)))
      (let loop ((i 0) (acc '()))
        (if (< i count)
            (let* ((extension-block* (GifFileType->ExtensionBlock gif* i))
                   (extension-block (ExtensionBlock->specialized-block extension-block*)))
              (loop (add1 i) (cons extension-block acc)))
            (reverse acc))))))

(define (gif-metadata gif)
  (assert-gif-slurped! gif 'gif-metadata)
  (let ((blocks (gif-extension-blocks gif)))
    (if (null? blocks)
        #f
        (append-map block-run->metadata (chunk-extension-blocks blocks)))))

(define (gif-frame-count gif)
  (assert-gif-slurped! gif 'gif-frame-count)
  (and-let* ((gif* (gif-pointer gif)))
    (GifFileType->ImageCount gif*)))

(define (gif-frame-ref gif index)
  (assert-gif-slurped! gif 'gif-frame-ref)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (if (and (>= index 0) (< index count))
          (make-frame #t (GifFileType->SavedImage gif* index))
          (abort (oob-error index count 'gif-frame-ref))))))

(define (gif-frame-for-each proc gif)
  (assert-gif-slurped! gif 'gif-frame-for-each)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (proc (make-frame #t (GifFileType->SavedImage gif* i)))
          (loop (add1 i)))))))

(define (gif-frame-for-each-indexed proc gif)
  (assert-gif-slurped! gif 'gif-frame-for-indexed)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (let loop ((i 0))
        (when (< i count)
          (proc (make-frame #t (GifFileType->SavedImage gif* i)) i)
          (loop (add1 i)))))))

(define (gif-frames gif)
  (assert-gif-slurped! gif 'gif-frames)
  (and-let* ((gif* (gif-pointer gif)))
    (let ((count (GifFileType->ImageCount gif*)))
      (let loop ((frames '())
                 (i 0))
        (if (< i count)
            (let ((frame (make-frame #t (GifFileType->SavedImage gif* i))))
              (loop (cons frame frames) (add1 i)))
            (reverse frames))))))

;;; color maps

(define (create-color-map size #!optional color-map)
  (if (and (>= size 0) (<= size 255))
      ;; TODO: test whether optional argument works
      (let ((color-map* (if color-map (ColorMapObject->Colors (color-map-pointer color-map)) #f)))
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

(define color-map-ref (getter-with-setter color-map-ref color-map-set!))

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

(define (color-map-colors color-map)
  (and-let* ((color-map* (color-map-pointer color-map))
             (count (ColorMapObject->ColorCount color-map*)))
    (let loop ((colors '())
               (i 0))
      (if (< i count)
          (let ((color (make-color (ColorMapObject->Color color-map* i))))
            (loop (cons color colors) (add1 i)))
          (reverse colors)))))

;;; colors

(define GifColorType-size (foreign-type-size (struct "GifColorType")))

(define (create-color #!optional red green blue)
  (let* ((color (make-color (make-locative (make-blob GifColorType-size))))
         (color* (color-pointer color)))
    ((foreign-lambda* void ((GifColorType* c) (uint8 r) (uint8 g) (uint8 b))
       "c->Red = r, c->Green = g, c->Blue = b;")
     color* (or red 0) (or green 0) (or blue 0))
    color))

(define (color-red color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Red color*)))

(define (color-red-set! color red)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Red-set! color* red)))

(define color-red (getter-with-setter color-red color-red-set!))

(define (color-green color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Green color*)))

(define (color-green-set! color green)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Green-set! color* green)))

(define color-green (getter-with-setter color-green color-green-set!))

(define (color-blue color)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Blue color*)))

(define (color-blue-set! color blue)
  (and-let* ((color* (color-pointer color)))
    (GifColorType->Blue-set! color* blue)))

(define color-blue (getter-with-setter color-blue color-blue-set!))

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

(define frame-width (getter-with-setter frame-width frame-width-set!))

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

(define frame-height (getter-with-setter frame-height frame-height-set!))

(define (frame-left frame)
  (SavedImage->Left (frame-pointer frame)))

(define (frame-left-set! frame left)
  (if (not (negative? left))
      (SavedImage->Left-set! (frame-pointer frame) left)
      (abort (type-error left "non-negative left" 'frame-left-set!))))

(define frame-left (getter-with-setter frame-left frame-left-set!))

(define (frame-top frame)
  (SavedImage->Top (frame-pointer frame)))

(define (frame-top-set! frame top)
  (if (not (negative? top))
      (SavedImage->Top-set! (frame-pointer frame) top)
      (abort (type-error top "non-negative top" 'frame-top-set!))))

(define frame-top (getter-with-setter frame-top frame-top-set!))

(define (frame-interlaced? frame)
  (SavedImage->Interlace (frame-pointer frame)))

(define (frame-interlaced?-set! frame interlaced?)
  (SavedImage->Interlace-set! (frame-pointer frame) interlaced?))

(define frame-interlaced? (getter-with-setter frame-interlaced? frame-interlaced?-set!))

(define (frame-color-map frame)
  (let ((color-map* (SavedImage->ColorMap (frame-pointer frame))))
    (if color-map*
        (make-color-map color-map*)
        #f)))

(define (frame-color-map-set! frame color-map)
  (and-let* ((frame* (frame-pointer frame))
             (color-map* (color-map-pointer color-map)))
    (SavedImage->ColorMap-set! frame* color-map*)))

(define frame-color-map (getter-with-setter frame-color-map frame-color-map-set!))

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

(define frame-pixel (getter-with-setter frame-pixel frame-pixel-set!))

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

(define frame-row (getter-with-setter frame-row frame-row-set!))

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

(define frame-rows (getter-with-setter frame-rows frame-rows-set!))

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

(define frame-pixels (getter-with-setter frame-pixels frame-pixels-set!))

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
        (else
         (case (disposal-strategy)
           ((#f) (abort (unknown-disposal-error disposal 'data->graphics-control-block)))
           ((#t) 'unknown)
           (else => (lambda (proc) (proc disposal))))))
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
           (else => (lambda (disposal) (abort (unknown-disposal-error disposal 'graphics-control-block->data))))))
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

;; TODO: write a convenience predicate whether an image is animated
