;
; pattern-fill-adjustment-layer
;
; Creates a pattern fill "adjustment layer". 
;
; Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; Define the function

(define (script-fu-pattern-fill-adjustment-layer inImage
                                             inLayer
                                             inPattern
                                             inReplaceLayer
        )
    (let* (
            (adjustment 0)
            (theLayer)
            (currentSelection 0)
            (currentPattern 0)
            (currentFgColor '(0 0 0))
            (currentColor '(0 0 0))
            (theOpacity 0)
            (adjustmentMask 0)
          )
        ; Make sure to work on the layer, not the layer mask
        (if (= (car (gimp-drawable-is-layer-mask inLayer)) TRUE)
            (set! theLayer (car (gimp-image-get-active-layer inImage)))
            (set! theLayer inLayer)
        )
        
        ; Group undo information
        (gimp-image-undo-group-start inImage)
        
        ; Create the adjustment layer
        (set! adjustment (car (gimp-layer-new inImage
                     (car (gimp-image-width inImage))
                     (car (gimp-image-height inImage))
                     (car (gimp-drawable-type-with-alpha inLayer))
                     "Pattern Fill"
                     100 0)))
        (gimp-image-insert-layer inImage adjustment 0 0)
        (if (= inReplaceLayer TRUE)
            (begin
                (gimp-layer-set-linked adjustment
                    (car (gimp-layer-get-linked theLayer)))
                (gimp-layer-set-opacity adjustment
                    (car (gimp-layer-get-opacity theLayer)))
            )
            ()
        )
        
        ; Create the effect
        (set! currentSelection (car (gimp-selection-save inImage)))
        ;(gimp-image-remove-channel inImage currentSelection)
        (gimp-selection-all inImage)
        (gimp-edit-clear adjustment)
        (set! currentPattern (car (gimp-patterns-get-pattern)))
        (gimp-context-set-pattern inPattern)
        (gimp-edit-bucket-fill adjustment 2 0 50 255 FALSE 0 0)
        (gimp-patterns-set-pattern currentPattern)
        ;(gimp-image-add-layer inImage adjustment -1)
        (if (= inReplaceLayer TRUE)
            (begin
                (gimp-image-remove-layer inImage theLayer)
                (if (= (car (gimp-drawable-is-layer-mask inLayer)) TRUE)
                    ;(gimp-selection-load inLayer)
                    (gimp-image-select-item inImage CHANNEL-OP-REPLACE inLayer)
                    (if (= (car (gimp-layer-mask theLayer)) -1)
                        ;(gimp-selection-none inImage)
                        ;(gimp-selection-load (car (gimp-layer-mask theLayer)))
                        (gimp-image-select-item inImage CHANNEL-OP-REPLACE (car (gimp-layer-get-mask theLayer)))
                    )
                )
            )
            (gimp-selection-load currentSelection)
        )
        
        ; Add a layer mask
        (if (= (car (gimp-selection-is-empty inImage)) FALSE)
            (begin
                (set! adjustmentMask (car (gimp-layer-create-mask adjustment 1)))
                (gimp-layer-add-mask adjustment adjustmentMask)
                (set! currentFgColor (car (gimp-palette-get-foreground)))
                (gimp-context-set-foreground '(255 255 255))
                (gimp-bucket-fill adjustmentMask 0 0 100 255 FALSE 0 0)
                (gimp-palette-set-foreground currentFgColor)
            )
            (begin
                (set! adjustmentMask (car (gimp-layer-create-mask adjustment 0)))
                (gimp-layer-add-mask adjustment adjustmentMask)
            )
        )
        ;(gimp-image-add-layer-mask inImage adjustment adjustmentMask)
        
        (if (= inReplaceLayer TRUE)
            ;(gimp-selection-load currentSelection)
            (gimp-image-select-item inImage CHANNEL-OP-REPLACE currentSelection)
            ()
        )
        
        ; Group undo information
        (gimp-image-undo-group-end inImage)
        
        ; Force update
        (gimp-displays-flush)
        
        ; Return
        (list 1)
    )
)
; Register script-fu-pattern-fill-adjustment-layer

(script-fu-register
    "script-fu-pattern-fill-adjustment-layer"
    "<Toolbox>/Script-Fu3/Adjustment Layers/Pattern Fill"
    "Creates a pattern fill \"adjustment layer\". \nfile:adjustment_pattern-Fill-adjustment-layer.scm"
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "7th November 2002"
    "RGB* GRAY*"
    SF-IMAGE      "The Image"             0
    SF-DRAWABLE   "The Layer"             0
    SF-PATTERN    "Pattern"               "Pine?"
    SF-TOGGLE     "Replace Layer"         FALSE
)
