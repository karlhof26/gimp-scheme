; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

;; This script adds a layermask to the active layer based upon 
;; the layermask of the visible layer above it in the layer stack. 
;; Specifically,
;;   If the active layer already has a mask, it is replaced.
;;   If the above layer has a mask, it is added to the active layer.
;;   If the above layer does not have a mask, a grayscale copy of the 
;;     above layer is added as a mask to the active layer -- unless
;;     the above layer is a text layer, in which case the text 
;;     is used for the mask.
;;   If there is no visible layer above the active one, a mask is added 
;;     initialized from the selection (avoiding the dialog). 
;

(define (script-fu-sg-copy-mask-from-above image layer)
    (gimp-image-undo-group-start image)
    (set! layer (car (gimp-image-get-active-layer image)))
    (unless (= (car (gimp-layer-get-mask layer)) -1)
        (gimp-image-remove-layer-mask image layer MASK-DISCARD) )
    (let* (
            (above-layer 
                (let loop ((above-layer #f)
                     (layers (vector->list (cadr (gimp-image-get-layers image)))) )
                    (if (null? layers)
                        above-layer
                        (begin
                            (if (= (car layers) layer)
                                (loop above-layer '())
                                (loop (if (zero? (car (gimp-drawable-get-visible (car layers))))
                                            above-layer
                                            (car layers) )
                                    (cdr layers) 
                                )
                            )
                        )
                    )
                )
            )
            (orig-sel (car (gimp-selection-save image)))
          )
        (when above-layer
            (let ((above-mask (car (gimp-layer-get-mask above-layer))))
                (if (= above-mask -1)
                    (if (zero? (car (gimp-drawable-is-text-layer above-layer)))
                        (begin
                            (set! above-mask (car (gimp-layer-create-mask above-layer ADD-MASK-COPY)))
                            (gimp-layer-add-mask above-layer above-mask)
                            (gimp-selection-load above-mask)
                            (gimp-layer-remove-mask above-layer MASK-DISCARD)
                        )
                        (gimp-selection-layer-alpha above-layer)
                    )
                    (gimp-selection-load above-mask)
                )
            )
        )
        (gimp-layer-add-mask layer (car (gimp-layer-create-mask layer ADD-MASK-SELECTION)))
        (gimp-selection-load orig-sel)
        (gimp-image-remove-channel image orig-sel)
        (gimp-image-set-active-layer image layer)
        (gimp-layer-set-edit-mask layer TRUE) 
    )
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
)

(script-fu-register "script-fu-sg-copy-mask-from-above"
    "Copy Layer Mask From Above"
    "Copy mask from above layer a la GAP's Modify Frames \nfile:sg-copy-mask-from-above.scm"
    "Saul Goode"
    "saulgoode"
    "February 2012"
    "*"
    SF-IMAGE    "Image"     0
    SF-DRAWABLE "Drawable"  0 ; to allow registering in <Layers> menu
)
  
(script-fu-menu-register "script-fu-sg-copy-mask-from-above"
  "<Layers>"
  )
(script-fu-menu-register "script-fu-sg-copy-mask-from-above"
  "<Toolbox>/Script-Fu/Layer/Mask"
  )

;end of script