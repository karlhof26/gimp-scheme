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

(define (kwardfog inimage indraw color turb opac)
    
    (let* (
            (theImage 0)
            (theDraw 0)
            (flat 0)
            (width 0)
            (height 0)
            (white-layer 0)
            (mask 0)
        ) 
        (set! theImage inimage)
        (set! theDraw indraw)   
        ;;set undo
        (gimp-image-undo-group-start theImage)
        
        (set! flat (car (gimp-image-flatten theImage)))
        (gimp-context-set-background color)
        (set! width (car (gimp-drawable-width flat)))
        (set! height (car (gimp-drawable-height flat)))
        (set! white-layer (car (gimp-layer-new theImage width height 1 "white" opac 0)))
        (gimp-drawable-fill white-layer FILL-BACKGROUND)
        (gimp-image-insert-layer theImage white-layer 0 -1)
        
        (set! mask (car (gimp-layer-create-mask white-layer 0)))
        (gimp-layer-add-mask white-layer mask)
        
        (plug-in-plasma 1 theImage mask (rand 429496) turb)
        
        (gimp-image-flatten theImage)
        
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    )
) 

(script-fu-register     "kwardfog"
            "<Toolbox>/Script-Fu/Effects/fog kward..."
            "Applies a fog of chosen colour over image \n file:kward1979uk_fog.scm"
            "Karl Ward"
            "Karl Ward"
            "Nov 2006"
            "RGB*"
            SF-IMAGE        "SF-IMAGE"      0
            SF-DRAWABLE     "SF-DRAWABLE"   0
            SF-COLOR        "Fog Colour"    '(255 255 255)
            SF-ADJUSTMENT   "Turbulance"    '(1.0 0 10 0.1 1 1 0)
            SF-ADJUSTMENT   "Opacity"       '(100 0 100 1 5 1 0)
)

;end of script
