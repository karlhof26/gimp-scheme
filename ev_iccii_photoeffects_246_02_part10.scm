; 
;*************************************************************************************** 
; Palette Knife image script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-paletteknife-ga
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
        (gimp-drawable-fill old-selection FILL-WHITE)
    ) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-drawable-fill layer-temp1 FILL-TRANSPARENT)
    (gimp-image-add-layer img layer-temp1 -1)
    (gimp-layer-add-alpha layer-temp1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (plug-in-gimpressionist 1 img layer-temp1 "ev_paletknife2")
    (gimp-drawable-levels layer-temp1 HISTOGRAM-VALUE 0.0 1.0 TRUE 0.5 0.0 1.0 TRUE) 
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Palette knife")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-paletteknife-ga"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist/Palette Knife..."
    "Creates a drawing effect like made with a palette knife, based on the Gimpressionist. \n: file:photoeffects_246_02_part10.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "RGB* GRAY*"
    SF-IMAGE      "Image"             0
    SF-DRAWABLE   "Drawable"          0
)

;*************************************************************************************** 
; Angled Strokes image script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-angledstrokes-ga
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
    
    (plug-in-gimpressionist 1 img layer-temp1 "ev_angledstrokes")
    (plug-in-unsharp-mask 1 img layer-temp1 5.0 1.0 0) 
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Angled strokes")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-angledstrokes-ga"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Brush/Angled strokes..."
    "Creates a drawing effect, based on the Gimpressionist. \n: file:photoeffects_246_02_part10.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "RGB* GRAY*"
    SF-IMAGE      "Image"               0
    SF-DRAWABLE   "Drawable"            0
)

; end of script