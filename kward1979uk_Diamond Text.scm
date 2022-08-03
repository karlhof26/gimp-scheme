; Updated for GIMP-2.10.22 by karlhof26
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (my-image-to-layer-size image layer)
    
    (gimp-layer-set-offsets layer 0 0)
    (gimp-image-resize image
            (car (gimp-drawable-width layer))
            (car (gimp-drawable-height layer))
            0 0)
)

(define (my-layer-add-border layer border)
    (let* (
            (width  (car (gimp-drawable-width  layer)))
            (height (car (gimp-drawable-height layer)))
          )
          (gimp-layer-resize layer
                    (+ width border) (+ height border)
                    (/ border 2) (/ border 2))
    )
)

(define (diamond-text text font font-size bevel)
    (let* (
            (image (car (gimp-image-new 256 256 RGB)))
            (text-layer (car (gimp-text-fontname image -1 0 0 text 0 TRUE font-size PIXELS
                    font)))
          )
        (my-image-to-layer-size image text-layer)
        
        (let* (
                (b-height (car (gimp-drawable-height text-layer)))
                (b-width (car (gimp-drawable-width text-layer)))
                (inset-layer)
                (new-layer)
              )
            (gimp-layer-set-lock-alpha  text-layer 1)
            (gimp-context-set-gradient  "Golden")
            
            (gimp-selection-layer-alpha text-layer)     
            (gimp-edit-blend text-layer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 0 FALSE TRUE 3 0.20 TRUE 0 0 0 b-height)
            
            (script-fu-add-bevel  image text-layer bevel 0 0)
            (gimp-selection-layer-alpha text-layer)
            (gimp-selection-shrink image 4)
            ;(gimp-message "line63")
            
            (set! inset-layer (gimp-layer-new image b-width b-height 1 "inset" 100 0))
            (gimp-drawable-fill (car inset-layer) 3)
            ;(gimp-message "line67") 
            (gimp-image-insert-layer image (car inset-layer) 0 -1)
            (gimp-edit-bucket-fill (car inset-layer) 1 LAYER-MODE-NORMAL 100 0 0 0 0 )
            
            (plug-in-rgb-noise 1 image (car inset-layer) 0 0 0.80 0.80 0.80 0)
            (plug-in-rgb-noise 1 image (car inset-layer) 0 0 0.10 0.10 0.10 0)
            (gimp-selection-none image)
            (plug-in-bump-map 1 image (car inset-layer) (car inset-layer) 112 12.5 46 -1 0 0.26 1.00 1 0 0)
            ;(gimp-message "line76")
            
            (set! new-layer (car (gimp-image-merge-visible-layers
                image EXPAND-AS-NECESSARY)))
            ;(gimp-message "line79")
            (my-image-to-layer-size image new-layer)
        )
        
        (gimp-display-new image)
        
        (gc); garbage cleaner
        
    )
)

(script-fu-register     "diamond-text"
    "<Image>/Script-Fu/Text/Diamond covered golden text"
    "Create gold text covered in diamonds. \nfile:kward1979uk_Diamond Text.scm"
    "Karl Ward"
    "Karl Ward"
    "October 2007"
    ""
    SF-STRING       "Text"      "Golden Text"
    SF-FONT         "Font"      "Arial Bold"
    SF-ADJUSTMENT   "Font-size" '(230 50 400 1 10 0 1)
    SF-ADJUSTMENT   "Bevel"     '(10 5 20 .1 1 1 0)
)

;end of script