; "BW Colorizer" is a script for The GIMP 
;
; This script helps you enhancing your black and white images with
; fine-tuned dominants.
;
; The script is located in "<Image> / Script-Fu / Enhance / BW Colorizer"
;
; Last changed: 2nd February 2010
;
; Copyright (C) 2010 Riccardo Traverso <gr3yfox.fw@gmail.com>
; Copyright (C) 2020 karlhof26 - improvements to work on Gimp 2.10.18
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.1
;    - Initial version
;  Version 0.2
;    - Improvements
; --------------------------------------------------------------------
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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;

(define (add-layer-over image
            add
            over)
    (let* (
            (index 0)
        ) 
        (gimp-image-set-active-layer image over)
        (gimp-image-insert-layer image add 0 -1)
    )
)


(define (script-fu-bw-colorizer
                theImage
                theLayer
                strength
                color
                layerMask
                mergeLayers)
        
    
  (let* (
           ; Create a new empty layer
           (thewidth (car(gimp-image-width theImage)))
           (overlayLayer (car
                 (gimp-layer-new theImage thewidth (car(gimp-image-height theImage)) RGBA-IMAGE "Overlay" strength LAYER-MODE-OVERLAY)
               )
           )
           (overlayMaskLayer 0)
           (overlayMask 0)
           ; Save the old foreground before changing it
           (oldFGColor (car(gimp-context-get-foreground)))
       )
       
    ; Enable undoing this script
    (gimp-image-undo-group-start theImage)
       
    ; Attach the overlay layer over the selected one
    (add-layer-over theImage overlayLayer theLayer)
        
    ; Set color and fill
    (gimp-context-set-foreground color)
    (gimp-drawable-fill overlayLayer  FILL-FOREGROUND)
    (gimp-context-set-foreground oldFGColor)
    
    ; Create and apply the layer mask to the overlay if required
    (if (= layerMask 1) 
        (begin
                    
                    (set! overlayMaskLayer (car (gimp-layer-copy theLayer FALSE)))
                    (set! overlayMask (car (gimp-layer-create-mask overlayLayer ADD-MASK-WHITE)))
                    
                (gimp-layer-add-mask overlayLayer overlayMask)
                (add-layer-over theImage overlayMaskLayer theLayer)
                (gimp-invert overlayMaskLayer)
                (gimp-edit-copy overlayMaskLayer)
                (gimp-edit-paste overlayMask TRUE)
                (gimp-floating-sel-anchor (car(gimp-image-get-floating-sel theImage)))
                (gimp-image-remove-layer theImage overlayMaskLayer)
            
        )
    )
    
    ; Merge the result into one single layer if required
    (if (= mergeLayers 1)
        (begin
            (gimp-image-merge-down theImage overlayLayer CLIP-TO-BOTTOM-LAYER)
        )
    )
    
    ; End undo group
    (gimp-image-undo-group-end theImage)
    
    ; Update image 
    (gimp-displays-flush)
    
  )
)


(script-fu-register "script-fu-bw-colorizer"
            "<Image>/Script-Fu3/Enhance/BW colorizer" ;"BW Colorizer"
            "Enhance your black and white images with fine-tuned dominants. \n file:bw-colorizer_02.scm"
            "Riccardo Traverso"
            "Copyright 2010, Riccardo Traverso"
            "February 2, 2010"
            ""
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT "Strength" '(10 0 100 1 10 0 0)
            SF-COLOR "Color" '(255 128 0)
            SF-TOGGLE "Layer mask" TRUE
            SF-TOGGLE "Merge layers" FALSE
)

; end of script
