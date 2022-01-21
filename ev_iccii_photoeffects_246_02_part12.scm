
;*************************************************************************************** 
; Cartoon script  for GIMP 2.10.18
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; Copyright (C) 2020 Karl Hofmeyr karlhof26 
; --------------------------------------------------------------------


(define (script-fu-cartoon-ga
            img
            drawable
            posterizeoption
            posterlevels
    )
    
  (gimp-image-undo-group-start img)
    
  (let* (
        (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (old-selection (car (gimp-selection-save img)))
        (image-type (car (gimp-image-base-type img)))
        (layer-type (car (gimp-drawable-type drawable)))
        (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL)))
        (layer-temp2 (car (gimp-layer-new img width height layer-type "temp2"  100 LAYER-MODE-NORMAL)))
        ) 
    
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-image-insert-layer img layer-temp2 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    
    (plug-in-photocopy 1 img layer-temp2 8.0 1.0 0.0 0.8)
   
    
    
    ;(gimp-levels layer-temp2 0 215 235 1.0 0 255) 
    (gimp-drawable-levels layer-temp2 HISTOGRAM-VALUE 0.0843 0.921 TRUE 1.0 0.0 1.0 TRUE)      ; 215 235 0 255  0.843 0.921
     (gimp-drawable-threshold layer-temp2 HISTOGRAM-LUMINANCE 0.92 1.0)
    (gimp-displays-flush)
    
    
    (gimp-layer-set-mode layer-temp2 LAYER-MODE-LUMINANCE)  ; was 3 ; or -LCH-LIGHTNESS
    ;(gimp-levels layer-temp1 0 25 225 2.25 0 255)
    (gimp-drawable-levels layer-temp1 HISTOGRAM-VALUE 0.098 0.882 TRUE 2.25 0.0 1.0 TRUE)
    
    (if (= posterizeoption 1)
        (begin
            (gimp-drawable-levels-stretch layer-temp1)
            (gimp-drawable-posterize layer-temp1 posterlevels)
        )
    )
    (gimp-displays-flush)
    ;(quit)
    
    (gimp-image-merge-down img layer-temp2 0)
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Cartoon")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-cartoon-ga"
    "Cartoon..."
    "Creates a line heavy cartoon.\n: file:photoeffects_246_02_part12.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "RGB* GRAY*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Drawable"          0
    SF-OPTION       "Setting" '("Default" "With Posterize")
    SF-ADJUSTMENT   "Posterize level"  '(3 2 10 1 2 0 0)
)

(script-fu-menu-register "script-fu-cartoon-ga" "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist") 

(script-fu-menu-register "script-fu-cartoon-ga" "<Toolbox>/Script-Fu3/Comics-o-matic")

;*************************************************************************************** 
; Cartoon-2 script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-cartoon2-ga
            img
            drawable
            colors
            smoothness
    )
    
  (gimp-image-undo-group-start img)
    
  (let* (
        (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (old-selection (car (gimp-selection-save img)))
        (image-type (car (gimp-image-base-type img)))
        (blur (* width  (* smoothness 0.002 )))
        (layer-type (car (gimp-drawable-type drawable)))
        (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL)))
        (layer-temp3 (car (gimp-layer-new img width height layer-type "temp3"  100 LAYER-MODE-NORMAL)))
        (layer-temp4 (car (gimp-layer-new img width height layer-type "temp4"  100 LAYER-MODE-NORMAL)))
        (img2 (car (gimp-image-new width height image-type)))
        (layer-temp2 (car (gimp-layer-new img2 width height layer-type "temp2"  100 LAYER-MODE-NORMAL)))
       ) 
        
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-drawable-fill layer-temp1 FILL-TRANSPARENT)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (plug-in-gauss 1 img layer-temp1 blur blur 0)
    (gimp-edit-copy layer-temp1)
    
    
    (gimp-image-insert-layer img2 layer-temp2 0 -1)
    (gimp-drawable-fill layer-temp2 FILL-TRANSPARENT)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-image-convert-indexed img2 0 0 colors 0 0 "0")
    (gimp-edit-copy layer-temp2)
    (gimp-image-delete img2)
    
    
    (gimp-layer-add-alpha layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    ;------------------------------------------------
    (gimp-image-insert-layer img layer-temp3 0 -1)
    (gimp-image-insert-layer img layer-temp4 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp3 0)))
    
    (gimp-drawable-desaturate layer-temp3 DESATURATE-LUMINANCE)
    (gimp-edit-copy layer-temp3)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp4 0)))
    (gimp-drawable-invert layer-temp4 FALSE)
    (plug-in-gauss 1 img layer-temp4 4 4 0)
    (gimp-layer-set-mode layer-temp4 16)
    (gimp-image-merge-down img layer-temp4 0)
    (set! layer-temp3 (car (gimp-image-get-active-layer img)))
    ;(gimp-levels layer-temp3 0 215 235 1.0 0 255) 
    (gimp-drawable-levels layer-temp3 HISTOGRAM-VALUE 0.0843 0.921 TRUE 1.0 0.0 1.0 TRUE)      ; 215 235 0 255  0.843 0.921
    
    (gimp-layer-set-mode layer-temp3 3)    
    ;------------------------------------------------
    (gimp-image-merge-down img layer-temp3 0)
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))
    
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Toon")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
   "script-fu-cartoon2-ga"
   "Cartoon2..."
   "Creates a cartoon drawing effect.  \n: file:photoeffects_246_02_part12.scm"
   "Eddy Verlinden <eddy_verlinden@hotmail.com>"
   "Eddy Verlinden"
   "2007, juli"
   "RGB* GRAY*"
   SF-IMAGE      "Image"             0
   SF-DRAWABLE   "Drawable"          0
   SF-ADJUSTMENT "Colors"            '(6 4 32 1 10 0 0)
   SF-ADJUSTMENT "Smoothness"        '(8 1 20 1 1 0 0)
)


(script-fu-menu-register "script-fu-cartoon2-ga" "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist") 

(script-fu-menu-register "script-fu-cartoon2-ga" "<Toolbox>/Script-Fu3/Comics-o-matic")

;**

; end of script