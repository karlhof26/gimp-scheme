
;*************************************************************************************** 
; Inkpen script  for GIMP 2.10.18
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; Copyright (C) 2020 Karl karlhof26
; --------------------------------------------------------------------


(define (script-fu-inkpen-ga
            img
            drawable
            color
            lightness
            lengthA
            outlinesYN
            gridYN
    )
    
    (gimp-image-undo-group-start img)
    
  (let* (
        (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (old-selection (car (gimp-selection-save img)))
        (image-type (car (gimp-image-base-type img)))
            (blur (* height  0.01 ))
            (gridcolor '(128 128 255))
        (layer-type (car (gimp-drawable-type drawable)))
        (layer-tempa (car (gimp-layer-new img width height layer-type "tempa"  100 LAYER-MODE-NORMAL)))
        (layer-tempb (car (gimp-layer-new img width height layer-type "tempb"  100 LAYER-MODE-NORMAL)))
        (layer-tempc (car (gimp-layer-new img width height layer-type "tempc"  100 LAYER-MODE-NORMAL)))
        (layer-tempd (car (gimp-layer-new img width height layer-type "tempd"  100 LAYER-MODE-NORMAL)))
        (layer-tempe (car (gimp-layer-new img width height layer-type "tempe"  100 LAYER-MODE-NORMAL)))
        (layer-tempf (car (gimp-layer-new img width height layer-type "tempf"  100 LAYER-MODE-NORMAL)))
        (layer-tempg (car (gimp-layer-new img width height layer-type "tempg"  100 LAYER-MODE-NORMAL)))
       ) 
    
    (if (equal? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)
    ) ; so Empty and All are the same.
    (gimp-selection-none img)
    
    ;------------------------------------------------
    
    
    (gimp-drawable-fill layer-tempb FILL-WHITE)
    (gimp-image-insert-layer img layer-tempb 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempb 0)))
    ;(gimp-hue-saturation layer-tempb 0 0 lightness 0)
    (gimp-drawable-hue-saturation layer-tempb HISTOGRAM-VALUE 0 lightness 0 0)
    (gimp-threshold layer-tempb 125 255)
    
    (gimp-drawable-fill layer-tempa FILL-WHITE)
    (gimp-image-insert-layer img layer-tempa 0 -1)
    (plug-in-randomize-hurl 1 img layer-tempa 25 1 1 10)
    (plug-in-mblur 1 img layer-tempa 0 lengthA 135 1 0)
    (gimp-threshold layer-tempa 215 230)
    
    (gimp-drawable-fill layer-tempd FILL-WHITE)
    (gimp-image-add-layer img layer-tempd -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempd 0)))
    (gimp-hue-saturation layer-tempd 0 0 lightness 0)
    (gimp-threshold layer-tempd 75 255)
    
    (gimp-drawable-fill layer-tempc FILL-WHITE)
    (gimp-image-add-layer img layer-tempc -1)
    (plug-in-randomize-hurl 1 img layer-tempc 25 1 1 10)
    (plug-in-mblur 1 img layer-tempc 0 lengthA 45 0 0)
    (gimp-threshold layer-tempc 215 230)
    
    (gimp-layer-set-mode layer-tempa 10)
    (gimp-layer-set-mode layer-tempc 10)
    
    (gimp-image-merge-down img layer-tempc 0)
    (set! layer-tempc (car (gimp-image-get-active-layer img)))
    (gimp-layer-set-mode layer-tempc 3)
    
    (gimp-image-merge-down img layer-tempa 0)
    (set! layer-tempa (car (gimp-image-get-active-layer img)))
    (gimp-layer-set-mode layer-tempa 0)
    
    
    (gimp-image-merge-down img layer-tempc 0)
    (set! layer-tempa (car (gimp-image-get-active-layer img)))
    (gimp-layer-set-mode layer-tempa LAYER-MODE-NORMAL)
    ;------------------------------------------------
    (if (equal? outlinesYN TRUE)
        (begin
            ;(gimp-message "outlines OK")
            (gimp-image-add-layer img layer-tempe -1)
            (gimp-edit-copy drawable)
            (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempe 0)))
            (plug-in-photocopy 1 img layer-tempe 5.0 1.0 0.0 0.8)
            (gimp-levels layer-tempe 0 215 235 1.0 0 255) 
            (gimp-layer-set-mode layer-tempe 3)
            (gimp-image-merge-down img layer-tempe 0)
            (set! layer-tempa (car (gimp-image-get-active-layer img)))
        )
      )
    ;-------------------------------------------------
    (gimp-context-set-foreground color)
    (gimp-drawable-fill layer-tempf 0)
    (gimp-image-insert-layer img layer-tempf 0 -1)
    (gimp-layer-set-mode layer-tempf 4)
    (gimp-image-merge-down img layer-tempf 0)
    (set! layer-tempa (car (gimp-image-get-active-layer img)))
    
    ;------------------------------------------------
    (if (equal? gridYN TRUE)
        (begin
            ;(gimp-message "grid T")
            (gimp-drawable-fill layer-tempg FILL-WHITE)
            (gimp-image-insert-layer img layer-tempg 0 -1)
            (gimp-image-lower-layer img layer-tempg)
            (plug-in-grid 1 img layer-tempg 1 16 8 gridcolor 64 1 16 8 gridcolor 64 0 2 6 gridcolor 128)
            (plug-in-gauss 1 img layer-tempg 0.5 0.5 0)
            
            (gimp-layer-set-mode layer-tempa 9)
            
            (gimp-image-merge-down img layer-tempa 0)
            (set! layer-tempa (car (gimp-image-get-active-layer img)))
        )
    )
    ;------------------------------------------------
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (equal? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-tempa)
        )
    )
    
    (gimp-layer-set-name layer-tempa "Inkpen")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-inkpen-ga"
  "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist/Inkpen..."
  "Creates a inkpen drawing effect. \n: file:photoeffects_246_02_part13.scm"
  "Eddy Verlinden <eddy_verlinden@hotmail.com>"
  "Eddy Verlinden"
  "2007, juli"
  "RGB* GRAY*"
  SF-IMAGE      "Image"             0
  SF-DRAWABLE   "Drawable"          0
  SF-COLOR      "Ink Color"       '(0 0 0)
  SF-ADJUSTMENT "Lightness"       '(0 -100 100 1 10 0 0)
  SF-ADJUSTMENT "Stroke length"   '(30 10 50 1 10 0 0)
  SF-TOGGLE     "Outlines"       TRUE
  SF-TOGGLE     "Blue grid"      TRUE
)


;*************************************************************************************** 
; Conte image script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
; Copyright (C) 2020 Karl karlhof26
; --------------------------------------------------------------------


(define (script-fu-conte-ga
            img
            drawable
            brightness
            contrast
            wild?
            canvas?
    )
    
    (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-tempa (car (gimp-layer-new img width height layer-type "tempa"  100 LAYER-MODE-NORMAL)))
            (layer-tempb (car (gimp-layer-new img width height layer-type "tempb"  100 LAYER-MODE-NORMAL)))
            (layer-tempc (car (gimp-layer-new img width height layer-type "tempc"  100 LAYER-MODE-NORMAL)))
            (layer-tempd (car (gimp-layer-new img width height layer-type "tempd"  100 LAYER-MODE-NORMAL)))
            (layer-tempe (car (gimp-layer-new img width height layer-type "tempe"  100 LAYER-MODE-NORMAL)))
            (img2 (car (gimp-image-new width height image-type)))
            (layer-temp2 (car (gimp-layer-new img2 width height layer-type "temp2" 100 LAYER-MODE-NORMAL)))
        ) 

    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
;-------------------------------------------------------
    (if (eqv? (car (gimp-palettes-get-list "conte_ev8")) 0)
    (begin
        (gimp-palette-new "conte_ev8")
        (gimp-palette-add-entry "conte_ev8" "1" '(117 96 91))
        (gimp-palette-add-entry "conte_ev8" "2" '(139 91 87))
        (gimp-palette-add-entry "conte_ev8" "3" '(164 91 85))
        (gimp-palette-add-entry "conte_ev8" "4" '(185 103 89))
        (gimp-palette-add-entry "conte_ev8" "5" '(240 238 239))
        (gimp-palette-add-entry "conte_ev8" "6" '(205 212 220))
        (gimp-palette-add-entry "conte_ev8" "7" '(90 93 100))
        (gimp-palette-add-entry "conte_ev8" "8" '(51 51 51))
    ))
    (if (> (car (gimp-palettes-get-list "conte_ev8 ")) 0)
        (gimp-message "There is/are palette(s) 'conte_ev8 *'. The best is to delete all palettes 'conte_ev8' (in the dialog 'palettes'). A new original will be created the next time this script is activated"))
    (if (> (car (gimp-palettes-get-list "conte_ev8#")) 0)
        (gimp-message "There is/are palette(s) 'conte_ev8#'. The best is to delete all palettes 'conte_ev8' (in the dialog 'palettes'). A new original will be created the next time this script is activated"))
;-------------------------------------------------------
    (gimp-drawable-fill layer-tempa FILL-TRANSPARENT)
    (gimp-image-add-layer img layer-tempa -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempa 0)))
    (gimp-drawable-fill layer-tempb FILL-TRANSPARENT)
    (gimp-image-add-layer img layer-tempb -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempb 0)))
    
    (plug-in-neon 1 img layer-tempa 5.0 0)
    (gimp-invert layer-tempa)
    (gimp-desaturate layer-tempa)
    
    (gimp-brightness-contrast layer-tempb (* brightness 1.25) (* contrast 1.25))
    (plug-in-gauss 1 img layer-tempb 2.0 2.0 0)
    (gimp-image-add-layer img layer-tempc -1)
    (gimp-edit-copy layer-tempb)
    
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempc 0)))
    (plug-in-gimpressionist 1 img layer-tempc "ev_strokes45r")
    (plug-in-dog 1 img layer-tempc 7.0 2.0 TRUE TRUE)
    (gimp-threshold layer-tempc 250 255)
    
    (gimp-layer-set-mode layer-tempc 3)
    (gimp-layer-set-mode layer-tempb 3)
    (gimp-image-merge-down img layer-tempc 0)
    (set! layer-tempb (car (gimp-image-get-active-layer img)))
    (gimp-image-merge-down img layer-tempb 0)
    (set! layer-tempa (car (gimp-image-get-active-layer img)))
    (gimp-edit-copy layer-tempa)
    
;    (set! img2 (car (gimp-image-new width height image-type)))
;    (set! layer-temp2 (car (gimp-layer-new img2 width height layer-type "temp2"  100 NORMAL-MODE)))
    (gimp-image-add-layer img2 layer-temp2 -1)
    (gimp-drawable-fill layer-temp2 FILL-TRANSPARENT)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-image-convert-indexed img2 0 4 0 0 0 "conte_ev8")
    (gimp-edit-copy layer-temp2)
    (gimp-image-delete img2)
    
    (gimp-layer-add-alpha layer-tempa)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempa 0)))
    (gimp-image-insert-layer img layer-tempd 0 -1)
    (gimp-edit-copy layer-tempa)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-tempd 0)))
    (if (eqv? wild? TRUE)
        (begin
            (plug-in-gimpressionist 1 img layer-tempd "graphite2")
        )
    )
    (gimp-layer-set-mode layer-tempd 19)
    (gimp-image-merge-down img layer-tempd 0)
    (set! layer-tempa (car (gimp-image-get-active-layer img)))
    
    (if (eqv? canvas? TRUE)
        (begin
            (gimp-image-add-layer img layer-tempe -1)
            (gimp-context-set-foreground '(234 220 190))
            (gimp-drawable-fill layer-tempe FILL-FOREGROUND)
            (plug-in-apply-canvas 1 img layer-tempe 1 1)
            (gimp-layer-set-mode layer-tempa 9)
            (gimp-image-lower-layer img layer-tempe)
            (gimp-image-merge-down img layer-tempa 0)
            (set! layer-tempa (car (gimp-image-get-active-layer img)))
       )
    )
    
    ;   -------------------------------------------------------
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-tempa)
        )
    )
    
    (gimp-layer-set-name layer-tempa "Conte")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-conte-ga"
  "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist/Conte..."
  "Creates an image that looks like a conte sketch.  \n: file:photoeffects_246_02_part13.scm"
  "Eddy Verlinden <eddy_verlinden@hotmail.com>"
  "Eddy Verlinden"
  "2007, juli"
  "RGB* GRAY*"
  SF-IMAGE      "Image"	            0
  SF-DRAWABLE   "Drawable"          0
  SF-ADJUSTMENT "Brightness"        '(50 -100 100 1 10 0 0)
  SF-ADJUSTMENT "Contrast"          '(80 -100 100 1 10 0 0)
  SF-TOGGLE     "Wild"          TRUE
  SF-TOGGLE     "Canvas"        TRUE
)


;*************************************************************************************** 
;*************************************************************************************** 
;*************************************************************************************** 
;*************************************************************************************** 
;*************************************************************************************** 
