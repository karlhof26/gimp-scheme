; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; Revised January 2013 to add a "Polygonal" option that attempts
; to straighten the lines between colors. This option can be quite
; slow but yields a very nice result.
;
; Updated for GIMP-2.10.22 on 01/11/2020 by karlhof26
;
;

(define (script-fu-sg-quantize orig-image drawable num-colors smoothing rounded polygonal mask-selected)
  (define (polygonalize image path)
    (gimp-image-undo-group-start image)
    (let ((new-path (car (gimp-vectors-new image "temp"))))
      (gimp-image-add-vectors image new-path -1)
      (let loop ((strokes (vector->list (cadr (gimp-vectors-get-strokes path)))))
        (if (null? strokes)
          new-path
          (let* ((stroke-info (gimp-vectors-stroke-get-points path (car strokes)))
                 (new-points (let point-loop ((all-points (vector->list (caddr stroke-info)))
                                              (anchors '()) )
                               (if (null? all-points)
                                 anchors
                                 (point-loop (cddr (cddddr all-points))
                                             (append anchors 
                                                     (list (caddr all-points)
                                                           (cadddr all-points)
                                                           (caddr all-points)
                                                           (cadddr all-points)
                                                           (caddr all-points)
                                                           (cadddr all-points) )))))))
            (unless (< (cadr stroke-info) 18) ; must at least be a triangle
              (gimp-vectors-stroke-new-from-points new-path  
                                                   (car stroke-info)  
                                                   (length new-points)
                                                   (list->vector new-points)
                                                   (cadddr stroke-info) ))
            (loop (cdr strokes)) ))
      )
    )
  )

  (let* (
            (layer 0)
            (q-image 0)
            (q-layer 0)
            (q-sel 0)
            (orig-sel 0)
            (floating-sel 0)
            (q-mask #f)
            (buffer "")
        )
        
    (gimp-image-undo-group-start orig-image)
    (set! orig-sel (car (gimp-selection-save orig-image)))
    (gimp-selection-none orig-image)
    (set! buffer (car (gimp-edit-named-copy drawable "temp")))
    (set! q-image (car (gimp-edit-named-paste-as-new buffer)))
    (gimp-image-undo-disable q-image)
    (gimp-buffer-delete buffer)
    (set! q-layer (car (gimp-image-get-active-layer q-image)))
    (set! buffer (car (gimp-edit-named-copy orig-sel "temp")))
    (set! q-sel (car (gimp-selection-save q-image)))
    (set! floating-sel (car (gimp-edit-named-paste q-sel buffer FALSE)))
    (gimp-buffer-delete buffer)
    (gimp-floating-sel-anchor floating-sel)
    (unless (zero? (car (gimp-drawable-has-alpha q-layer)))
      (set! q-mask (car (gimp-layer-create-mask q-layer ADD-MASK-ALPHA-TRANSFER)))
      (gimp-layer-add-mask q-layer q-mask) )
    (unless (zero? mask-selected)
      (gimp-selection-load q-sel)
      (gimp-selection-invert q-image) )
    (if (zero? rounded)
      (begin
        (unless (zero? smoothing)
          (plug-in-gauss RUN-NONINTERACTIVE q-image q-layer smoothing smoothing 0) )
        (gimp-image-convert-indexed q-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-colors FALSE FALSE "")
        )
      (begin
        (gimp-image-convert-indexed q-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-colors FALSE FALSE "")
        (gimp-image-convert-rgb q-image)
        (unless (zero? smoothing)
          (plug-in-gauss RUN-NONINTERACTIVE q-image q-layer smoothing smoothing 0) )
        (gimp-image-convert-indexed q-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-colors FALSE FALSE "")
      )
    )
    (let ((colors (vector->list (cadr (gimp-image-get-colormap q-image)))))
      (gimp-image-convert-rgb q-image) 
      (gimp-selection-none q-image)
      (unless (zero? polygonal)
        (gimp-context-push)
        (let ((new-layer (car (gimp-layer-new q-image 
                                              (car (gimp-drawable-width q-layer))
                                              (car (gimp-drawable-height q-layer))
                                              RGBA-IMAGE
                                              "Fill"
                                              100
                                              LAYER-MODE-NORMAL ))))
          (gimp-drawable-fill new-layer FILL-TRANSPARENT)
          (gimp-image-insert-layer q-image new-layer 0 -1)
          (let ((prog-max (length colors)))
            (let colors-loop ((colors colors)
                              (prog-current 0) )
              (unless (null? colors)       
                (gimp-context-set-foreground (list (car colors) (cadr colors) (caddr colors)))
                (gimp-image-select-color q-image CHANNEL-OP-REPLACE q-layer (list (car colors)
                                                                                  (cadr colors)
                                                                                  (caddr colors) ))
                (plug-in-sel2path 1 q-image q-layer)
                (gimp-progress-set-text "Converting to paths")
                (gimp-progress-update (/ prog-current prog-max))
                (gimp-image-select-item q-image 
                                        CHANNEL-OP-REPLACE 
                                        (polygonalize q-image (car (gimp-image-get-active-vectors q-image))) )
                (gimp-edit-fill new-layer FILL-FOREGROUND)
                (colors-loop (cdddr colors) (+ prog-current 3)) )))
          (gimp-progress-set-text "Filling gaps")
          
          (gimp-selection-none q-image)
          
          (while (< (car (gimp-drawable-histogram new-layer HISTOGRAM-ALPHA 0.0 1.0)) 1.0)
            (plug-in-vpropagate RUN-NONINTERACTIVE
                                q-image
                                new-layer 
                                6 ; more opaque
                                3 ; with alpha
                                1.0 ; maximum amount
                                15 ; all directions
                                0
                                255 ))
          (when q-mask
            (gimp-selection-load (car (gimp-layer-get-mask q-layer)))
            (gimp-layer-add-mask new-layer (car (gimp-layer-create-mask new-layer ADD-MASK-SELECTION)))
            (gimp-selection-none q-image)
          )
          (gimp-image-remove-layer q-image q-layer)
          (set! q-layer new-layer) )
        (gimp-context-pop) ))
    (when q-mask
      (gimp-layer-remove-mask q-layer MASK-APPLY)
    )
    (set! buffer (car (gimp-edit-named-copy q-layer "temp")))
    (set! floating-sel (car (gimp-edit-named-paste drawable buffer TRUE)))
    (when (zero? mask-selected)
      (gimp-selection-load orig-sel)
    )
    (gimp-floating-sel-anchor floating-sel)
    (gimp-image-undo-enable q-image)
    (gimp-image-delete q-image)
    (gimp-selection-load orig-sel)
    (gimp-image-remove-channel orig-image orig-sel)
    (gimp-buffer-delete buffer)
    (gimp-progress-end)
    (gimp-image-undo-group-end orig-image)
    (gimp-displays-flush)
    )
  )
       
(script-fu-register "script-fu-sg-quantize"
    "Quantize..."
    "Quantize layer to a specified number of colors. \n file:sg-quantize.scm"
    "Saul Goode"
    "Saul Goode"
    "May 2011"
    "RGB*, GRAY*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Layer"             0
    SF-ADJUSTMENT   "Number of colors"  '(16 2 256 1 10 0 0)
    SF-ADJUSTMENT   "Smoothing"         '(0 0 200 1 10 0 0)
    SF-TOGGLE       "Rounded?"          TRUE
    SF-TOGGLE       "Polygonal? (slow)" FALSE
    SF-TOGGLE       "Use selection as smoothing mask"   FALSE
)

(script-fu-menu-register "script-fu-sg-quantize"
      "<Toolbox>/Script-Fu/Colors"
 )

;end of script