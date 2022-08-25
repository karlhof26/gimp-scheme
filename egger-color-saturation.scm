;;
; The GIMP -- an image manipulation program 
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Color Saturation script for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland 
; 
; Tags: saturation, color 
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  august 2007 - fixed for gimp 2.4
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Define the function
;
(define (script-fu-eg-colorsaturation-a inImage inLayer InIntensity InFlatten)
    
    ; Save history
    (gimp-image-undo-group-start inImage)
    ;
    (let* (
            (factor 2) ; was 0.025
            (plus 1)
            (minus 1.1)
            (ColorLayer (car (gimp-layer-copy inLayer TRUE)))
        )
       ; (gimp-message "ColorSat start")
        (set! factor (* InIntensity 0.50))
        (set! plus (+ 1 (* 2 factor)))
        (set! minus (* -1 factor))
        
       ; (gimp-message (number->string factor))
       ; (gimp-message (number->string plus))
       ; (gimp-message (number->string minus))
        (if (> plus 2)
            (begin
                ;(gimp-message "plus gt 2 out of range")
                (set! plus 2.0)
            )
        )
       (if (< plus -2)
            (begin
                ;(gimp-message "plus lt -2 out of range")
                (set! plus -2.0)
            )
        )
        (if (> minus 2)
            (begin
                ;(gimp-message "minus gt 2 out of range")
                (set! minus 2.0)
            )
        )
        (if (< minus -2)
            (begin
                ;(gimp-message "minus lt -2 out of range")
                (set! minus -2.0)
            )
        )
        
        (gimp-image-insert-layer inImage ColorLayer 0 -1)
        ;
        ; Apply new color mappings to image
        ;
        (plug-in-colors-channel-mixer RUN-NONINTERACTIVE inImage ColorLayer FALSE plus minus minus minus plus minus minus minus plus)
        (gimp-message "ColorSat after mix")
        ;
        ; Flatten the image, if we need to
        ;
        (cond
            ((= InFlatten TRUE)
                (gimp-image-merge-down inImage ColorLayer CLIP-TO-IMAGE)
            )
            ((= InFlatten FALSE)
                (gimp-item-set-name ColorLayer "Saturated")
            )
        )
        
        ;
        ; Finish work
        
        (gimp-message "ColorSat OK finished")
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush)
    )
)

; Register the function with the GIMP
;
(script-fu-register "script-fu-eg-colorsaturation-a"
    "<Toolbox>/Script-Fu/Colors/Eg Color Saturation A"
    "Saturate or desaturate color images. \nfile:egger-color-saturation.scm"
    "Martin Egger"
    "2005, Martin Egger, Bern, Switzerland"
    "15.05.2005"
    "RGB*"
    SF-IMAGE        "The Image"         0
    SF-DRAWABLE     "The Layer"         0
    SF-ADJUSTMENT   "Intensity"         '(1 -2 2 0.1 0 2 0)
    SF-TOGGLE       "Flatten Image"     FALSE
)

; end of script 
