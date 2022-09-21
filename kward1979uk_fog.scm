; GPLv3 
; The GIMP -- an image manipulation program   
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
; Copyright (C) 2010 Karl Ward
;

(define (kwardfog inimage indraw color turb opac noiseseed)
    
    (let* (
            (theImage 0)
            (theDraw 0)
            (flat 0)
            (width 0)
            (height 0)
            (white-layer 0)
            (mask 0)
            (savedsel 0)
        ) 
        (set! theImage inimage)
        (set! theDraw indraw)   
        ;;set undo
        (gimp-image-undo-group-start theImage)
        
        ;(set! flat (car (gimp-image-flatten theImage)))
        ;(gimp-message "line41")
        (if (= (car (gimp-selection-is-empty inimage)) FALSE)
            (begin
                ;(gimp-message "line44")
                (set! savedsel (car (gimp-selection-save theImage)))
            )
        )
        ;(gimp-message "line48")
        (gimp-context-set-background color)
        (set! width (car (gimp-drawable-width theDraw)))
        (set! height (car (gimp-drawable-height theDraw)))
        
        (set! white-layer (car (gimp-layer-new theImage width height 1 "white" opac 0)))
        
        ;(gimp-selection-none theImage)
        (gimp-drawable-fill white-layer FILL-BACKGROUND)
        (gimp-image-insert-layer theImage white-layer 0 -1)
        
        (set! mask (car (gimp-layer-create-mask white-layer ADD-MASK-BLACK)))
        (gimp-layer-add-mask white-layer mask)
        ;(gimp-message "line61")
        
        ;(plug-in-solid-noise 1 theImage white-layer TRUE TRUE 200001 4 4 4) 
        (if (= noiseseed 0)
            (set! noiseseed (rand 9500))
        )
        (plug-in-solid-noise 1 theImage mask FALSE FALSE noiseseed turb (+ turb 4) (+ turb 4))
        ;(plug-in-solid-noise 1 theImage mask FALSE TRUE noiseseed turb 1 1)
        (gimp-drawable-levels mask HISTOGRAM-VALUE 0.0 0.65 TRUE 2.25 0.0 1.0 TRUE)
        (gimp-displays-flush)
        
        
        ; plasma has a bug an cannot work with selections - result is always displaced by the selection amounts
        ;(plug-in-plasma 1 theImage mask (rand 429496) turb) 
        
        ;(gimp-image-flatten theImage)
        
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
        (gc) ; memory cleanup; garbage cleanup
    )
) 

(script-fu-register     "kwardfog"
            "<Toolbox>/Script-Fu/Effects/Fog kward..."
            "Applies a fog of chosen colour over image. This version allows selections and is repeatable. \n file:kward1979uk_fog.scm"
            "Karl Ward"
            "Karl Ward"
            "Nov 2006"
            "RGB*"
            SF-IMAGE        "SF-IMAGE"      0
            SF-DRAWABLE     "SF-DRAWABLE"   0
            SF-COLOR        "Fog Colour"    '(255 255 255)
            SF-ADJUSTMENT   "Turbulance"    '(1.0 0 10 0.1 1 1 0)
            SF-ADJUSTMENT   "Opacity"       '(100 0 100 1 5 1 0)
            SF-ADJUSTMENT   "Noise Seed 0=random"       '(5226 0 9500 1 10 0 0)
)

;end of script
