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
; Updated for GIMP-2.10.22 by karlhof26 (24/01/2021)
;

(define (set-pt arr i x y) 
    (aset arr (* i 2) x)
    (aset arr (+ (* i 2) 1) y)
)
(define (glass-sketch)
  (let* (
            (a (cons-array 18 'byte))
        )
        (set-pt a 0 0 0)
        (set-pt a 1 127 32)
        (set-pt a 2 255 64)
        a
  )
)

(define (kward-glass-kvar inimage indraw thickness displace delete)
    
    
    (gimp-context-set-foreground '( 255 255 255))
    (let* (
            (theImage inimage)
            (theDraw indraw)
            
            (height (car (gimp-image-height theImage)))
            (width (car (gimp-image-width theImage)))
            (layer2 (car (gimp-layer-copy theDraw 1)))
            (layer3 (car (gimp-layer-new theImage width height 1 "layer3" 100 0)))
        )
        (gimp-image-undo-group-start theImage)
        (gimp-image-insert-layer theImage layer2 0 -1)
        
        (gimp-drawable-fill layer3 3)
        (gimp-image-insert-layer theImage layer3 0 -1)
        (gimp-selection-layer-alpha layer2 )
        (gimp-edit-bucket-fill layer2 0 0 100 0 0 0 0)
        
        (gimp-layer-set-mode layer2 LAYER-MODE-OVERLAY)
        
        (gimp-context-set-foreground '( 132 106 79))
        (gimp-edit-bucket-fill layer3 0 0 100 0 0 0 0)
        (gimp-selection-none theImage)
        (plug-in-gauss 1 theImage layer2 5 5 1)
        (plug-in-bump-map 1 theImage layer3 layer2 135 45 (+ thickness 2) 0 0 0 0 0 0 0)
        (gimp-selection-layer-alpha layer3 )
        (gimp-selection-shrink theImage thickness)
        (gimp-selection-feather theImage (- thickness 1))
        (gimp-curves-spline layer3 4 6 (glass-sketch))
        (let* (
                (layer4 (car (gimp-layer-copy layer3 1)))
            )
            (gimp-image-insert-layer theImage layer4 0 -1)
            (gimp-edit-clear layer3)
            (gimp-selection-none theImage)
            (gimp-invert layer4)
            (plug-in-gauss 1 theImage layer3 (+ 2 displace) (+ 2 displace) 1)
            (gimp-drawable-offset layer3 1 0 displace displace)
            (gimp-hue-saturation layer4 0 0 0 -100)
            (gimp-image-remove-layer theImage layer2)
            ;(gimp-drawable-set-visible theDraw 0)
            (if (= delete TRUE) (gimp-image-remove-layer theImage theDraw))
            (gimp-layer-set-mode layer3 LAYER-MODE-BURN-LEGACY)
            (gimp-layer-set-opacity layer3 86)
            (gimp-image-undo-group-end theImage)
            (gimp-displays-flush)
            (gc) ; cleanup - an array was used
        )
    )
)

(script-fu-register  "kward-glass-kvar"
            "<Toolbox>/Script-Fu/Distorts/Glass..."
            "Turns a layer into glass. Works well with an Alpha layer and a selection. \nfile:kward1979uk_glass_script_fu_runs_on_2_4_by_kward1979uk.scm"
            "Karl Ward"
            "Karl Ward"
            "May 2008"
            "RGBA"
            SF-IMAGE        "SF-IMAGE" 0
            SF-DRAWABLE     "SF-DRAWABLE" 0
            SF-ADJUSTMENT   "Glass Thickness" '(5 1 20 1 2 0 1)
            SF-ADJUSTMENT   "Shadow Displacment" '(12 5 24 1 2 0 1)
            SF-TOGGLE       "Delete original layer" FALSE
            
)

; end of script