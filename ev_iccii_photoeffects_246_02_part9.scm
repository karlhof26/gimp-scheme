;***************************************************************************************
; Cutout image script for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
;
; --------------------------------------------------------------------
; Fidelity added by Rob Antonishen
; Fidelity added by karlhof26 

(define (script-fu-cutout-ga
        img
        drawable
        colors
        smoothness
        fidelity
        )
    
    (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (blur (/ (* width smoothness 0.001 ) fidelity))
            (count 0)
            (layer-lock (car (gimp-layer-get-lock-alpha drawable)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1" 100 LAYER-MODE-NORMAL)))
            (img2 (car (gimp-image-new width height image-type)))
            (layer-temp2 (car (gimp-layer-new img2 width height layer-type "temp2" 100 LAYER-MODE-NORMAL)))
    )
    
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
    (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-drawable-fill layer-temp1 FILL-TRANSPARENT)
    (gimp-image-add-layer img layer-temp1 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    
    (gimp-layer-set-lock-alpha layer-temp1 TRUE)
    (while (< count fidelity)
        (plug-in-gauss 1 img layer-temp1 blur blur 0)
        (set! count (+ count 1))
    )
    (gimp-layer-set-lock-alpha layer-temp1 layer-lock)
    
    (gimp-edit-copy layer-temp1)
    
    (gimp-image-add-layer img2 layer-temp2 -1)
    (gimp-drawable-fill layer-temp2 FILL-TRANSPARENT)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-image-convert-indexed img2 0 0 colors 0 0 "0")
    (gimp-edit-copy layer-temp2)
    (gimp-image-delete img2)
    
    (gimp-layer-add-alpha layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Cutout")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-cutout-ga"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist/Cutout Poster..."
    "Creates a Poster drawing effect.  \n file: photoeffects_246_02_part9.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "RGB* GRAY*"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-ADJUSTMENT "Colors" '(10 4 32 1 10 0 0)
    SF-ADJUSTMENT "Smoothness" '(8 1 20 1 1 0 0)
    SF-ADJUSTMENT "Fidelity" '(5 1 20 1 1 0 0)
)
    

;*************************************************************************************** 
; Color pencil image script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-coloredpencil-ga
            img
            drawable
    )
    
    (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL)))
        ) 
    
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same. 
    (gimp-selection-none img)
    (gimp-drawable-fill layer-temp1 FILL-TRANSPARENT)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-layer-add-alpha layer-temp1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (plug-in-gimpressionist 1 img layer-temp1 "ev_coloredpencil")
    ;(plug-in-gimpressionist 1 img layer-temp1 "wormcan")
    ;(gimp-hue-saturation layer-temp1 0 0 0 60)
    (gimp-drawable-hue-saturation layer-temp1 HISTOGRAM-VALUE 0.0 0.0 61.1 0.0)
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Col Pencil")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-coloredpencil-ga"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist/Color Pencil..."
    "Creates a drawing effect like made with colored pencils. Based on Gimpressionist. \n file: photoeffects_246_02_part9.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "RGB* GRAY*"
    SF-IMAGE      "Image"             0
    SF-DRAWABLE   "Drawable"          0
)

; end of script 