(module giflib
  (disposal-strategy
   gif? open-gif create-gif slurp-gif spew-gif close-gif
   gif-width gif-height gif-resolution gif-bg-index gif-aspect-ratio
   gif-width-set! gif-height-set! gif-resolution-set! gif-bg-index-set! gif-aspect-ratio-set!
   gif-color-map gif-color-map-set! create-color-map color-map? color-map-resolution color-map-sorted?
   color-map-count color-map-ref color-map-set! color-map-set*! color-map-for-each color-map-for-each-indexed color-map-colors
   color? create-color
   color-red color-red-set! color-green color-green-set! color-blue color-blue-set!
   gif-append-extension-block! gif-extension-block-count gif-extension-block-ref gif-extension-block-for-each gif-extension-block-for-each-indexed gif-extension-blocks gif-metadata
   gif-frame-count gif-frame-ref gif-frame-for-each gif-frame-for-each-indexed gif-frames
   frame? gif-append-frame! frame-width frame-width-set! frame-height frame-height-set! frame-left frame-left-set! frame-top frame-top-set! frame-interlaced? frame-interlaced?-set! frame-color-map frame-color-map-set! frame-pixel frame-pixel-set! frame-row frame-row-set! frame-rows frame-rows-set! frame-pixels frame-pixels-set!
   frame-append-extension-block! frame-extension-block-count frame-extension-block-ref frame-extension-block-for-each frame-extension-block-for-each-indexed frame-extension-blocks frame-metadata
   sub-block? make-sub-block sub-block-id sub-block-data
   comment-block? make-comment-block comment-block-text
   graphics-control-block? make-graphics-control-block graphics-control-block-disposal graphics-control-block-user-input? graphics-control-block-delay graphics-control-block-transparency-index
   text-block? make-text-block text-block-grid-left text-block-grid-top text-block-grid-width text-block-grid-height text-block-cell-width text-block-cell-height text-block-fg-index text-block-bg-index
   application-block? make-application-block application-block-identifier application-block-auth-code)

  (import scheme)
  (cond-expand
   (chicken-4
    (import chicken foreign)
    (use extras srfi-1 srfi-4 bitstring lolevel))
   (chicken-5
    (import (chicken base))
    (import (chicken blob))
    (import (chicken condition))
    (import (chicken foreign))
    (import (chicken format))
    (import (chicken gc))
    (import (chicken memory))
    (import (chicken locative))
    (import (srfi 1))
    (import (srfi 4))
    (import bitstring)
    (import miscmacros)))

  (include "giflib-impl.scm"))
