; The GIMP -- an image manipulation program ;
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
; 
; Copyright (C) 2008 elsamuko <elsamuko@web.de>
;
; Strong and inverted option added by karlhof26 2022/08/28

(define (elsamuko-color-tint aimg adraw color opacity saturation blackwhite invertcolorarea strong)
  (let* (
         (img (car (gimp-item-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         (tint-layer 0)
         (tint-layer-mask 0)
         
         (imgTMP 0)
         (copy-layer 0) ;first layer of imgTMP
         (tmp-layer 0)  ;tint layer of imgTMP
         (tmp-layer-mask 0)
         
         (imgHSV 0)
         (layersHSV 0)
         (layerS 0)
         
         (red   (/ (car   color) 255))
         (blue  (/ (cadr  color) 255))
         (green (/ (caddr color) 255))
        )
    
    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
    )
    
    ;filter ops on new image
    (gimp-edit-copy-visible img)
    (set! imgTMP (car (gimp-edit-paste-as-new)))
    
    ;rise saturation
    (set! copy-layer (car (gimp-image-get-active-layer imgTMP)))
    (gimp-drawable-hue-saturation copy-layer HUE-RANGE-ALL 0.0 2.0 saturation 1.0)
    
    ;add tint layer and filter color
    (set! tmp-layer (car (gimp-layer-copy copy-layer FALSE)))
    (gimp-item-set-name tmp-layer "Temp")
    (gimp-image-insert-layer imgTMP tmp-layer 0 -1)
    (plug-in-colors-channel-mixer 1 img tmp-layer TRUE
                                  red blue green ;R
                                  0 0 0 ;G
                                  0 0 0 ;B
                                  )
    
    ;add filter mask
    (set! tmp-layer-mask (car (gimp-layer-create-mask tmp-layer ADD-MASK-COPY)))
    (gimp-layer-add-mask tmp-layer tmp-layer-mask)
    
    ;colorize tint layer
    (gimp-context-set-foreground color)
    (gimp-selection-all imgTMP)
    (gimp-edit-bucket-fill tmp-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
    
    ;get visible and add to original
    (gimp-item-set-visible copy-layer FALSE)
    (gimp-edit-copy-visible imgTMP)
    (set! tint-layer (car (gimp-layer-new-from-visible imgTMP img "Tint") ))
    (gimp-image-insert-layer img tint-layer 0 0)
    
    ;set modes
    (cond ((= strong 0)
            (gimp-layer-set-mode tint-layer LAYER-MODE-SCREEN)
        )
        ((= strong 1)
            (gimp-layer-set-mode tint-layer LAYER-MODE-DODGE)
        )
        ((= strong 2)
            (gimp-layer-set-mode tint-layer LAYER-MODE-LUMA-DARKEN-ONLY)
        )
        ((= strong 3)
            (gimp-layer-set-mode tint-layer LAYER-MODE-DARKEN-ONLY)
        )
        ((= strong 4)
            (gimp-layer-set-mode tint-layer LAYER-MODE-LINEAR-BURN)
        )
        (else
            (gimp-layer-set-mode tint-layer LAYER-MODE-BURN)
        )
    )
    (gimp-layer-set-opacity tint-layer opacity)
    
    ;get saturation layer 
    (set! imgHSV (car (plug-in-decompose 1 imgTMP copy-layer "HSV" TRUE)))
    (set! layersHSV (gimp-image-get-layers imgHSV))
    (set! layerS (aref (cadr layersHSV) 1))
    (gimp-edit-copy layerS)
    
    ;add saturation mask
    (set! tint-layer-mask (car (gimp-layer-create-mask tint-layer ADD-MASK-WHITE )))
    (gimp-layer-add-mask tint-layer tint-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste tint-layer-mask TRUE)))
    
    ;desaturate original
    (if (= blackwhite TRUE) 
        (begin 
           (gimp-drawable-desaturate adraw DESATURATE-LUMINANCE)
        )
    )
    (gimp-drawable-invert tint-layer-mask FALSE)
    (if (= invertcolorarea TRUE)
        (begin
            ; remove mask
            ;(gimp-message "swapping")
            (gimp-image-remove-layer-mask img tint-layer MASK-DISCARD)
            (plug-in-threshold-alpha 1 img tint-layer 15)
            (gimp-context-set-foreground '(128 129 130))
            (gimp-context-set-background '(0 0 0))
            (gimp-context-set-sample-threshold-int 16)
            (gimp-context-set-sample-transparent TRUE)
            ; set what is not the color to a swap color - gray
            (gimp-image-select-color img CHANNEL-OP-REPLACE tint-layer color)
            (gimp-selection-invert img)
            (gimp-drawable-edit-fill tint-layer FILL-FOREGROUND)
            ; invert back and delete all existing color
            (gimp-selection-invert img)
            (gimp-edit-clear tint-layer)
            ;now fill selection with the color - swap color switches to color
            (gimp-selection-invert img)
            (gimp-context-set-foreground color)
            (gimp-drawable-edit-fill tint-layer FILL-FOREGROUND)
            (gimp-displays-flush)
            (gimp-selection-none img)
            
            (if (or (= strong 3) (= strong 4))
                (begin
                    (gimp-layer-set-mode tint-layer LAYER-MODE-SCREEN)
                )
            )
        )
    )
    
    (gimp-drawable-hue-saturation adraw HUE-RANGE-ALL 0.0 10.0 saturation 1.0)
    
    ; tidy up
    ;(gimp-display-new imgTMP)
    ;(gimp-display-new imgHSV)
    (gimp-image-delete imgTMP)
    (gimp-image-delete imgHSV)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

(script-fu-register "elsamuko-color-tint"
                    "Elsamuko Color Tint"
                    "Add color tint layer. Strong sets Dodge mode. Try deactivating mask for effect.\nfile: elsamuko-color-tint.scm"
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "16/04/10"
                    "*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    SF-COLOR       "Color"              '(0 0 255)
                    SF-ADJUSTMENT  "Opacity"            '(100 0 100 5 10 0 0)
                    SF-ADJUSTMENT  "Saturation"         '(100 0 100 5 10 0 0)
                    SF-TOGGLE      "Desaturate Image"    FALSE
                    SF-TOGGLE      "Invert color area"         FALSE
                    SF-OPTION      "Output mode"              '("Basic - Screen" "Strong - Dodge" "Dramatic - Luma darken only" "Dark - Darken only" "Burn - Linear Burn")
                    )

(script-fu-menu-register "elsamuko-color-tint"  "<Toolbox>/Script-Fu/Colors")
