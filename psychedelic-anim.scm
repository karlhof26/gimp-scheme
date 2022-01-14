; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Psychedelic Animation script  for GIMP 2.10.18
;
; Tags: animation psychedelic
;
; Author statement: 
;
; 
;   - Changelog -
;
; Updated to work with Gimp2.10.18 (05-2020)
;
; --------------------------------------------------------------------
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-psychedelic-anim 
                    image
                    layer
                    layers-added
                    blur
                    posterize
                    sharpen
                    playback)
    
    (let* (
            (number 0)
            (half-way 0)
            (up-down 0)
            (option 0)
          )
        
        (if (> layers-added 1)
            (begin
                
                (set! layers-added (inexact->exact (* (ceiling (/ layers-added 2)) 2)))
                (set! number (- layers-added 1))
                (set! half-way (/ layers-added 2))
            )
        )
        
        (gimp-image-undo-group-start image)
        
        (while (> layers-added 1)
            
            (gimp-image-add-layer image (car (gimp-layer-new-from-drawable layer image)) FALSE)
            
            (set! layers-added (- layers-added 1))
        )
        
        (while (> number -1)
            
            (plug-in-gauss 1 image (vector-ref (cadr (gimp-image-get-layers image)) number) (+ blur up-down) (+ blur up-down) 1)
            (gimp-drawable-posterize (vector-ref (cadr (gimp-image-get-layers image)) number) posterize)
            (plug-in-sharpen 1 image (vector-ref (cadr (gimp-image-get-layers image)) number) sharpen)
            ;(gimp-levels (vector-ref (cadr (gimp-image-get-layers image)) number) 0 0 160 1 0 255)
            (gimp-drawable-levels (vector-ref (cadr (gimp-image-get-layers image)) number) HISTOGRAM-VALUE 0.0 0.65 TRUE 1.0 0.0 1.0 TRUE)
            
            (if (= number half-way) (set! option 1))
            
            (case option
                ((0)    (set! up-down (+ up-down 1)))
                ((1)    (set! up-down (- up-down 1)))
            )
            
            (set! number (- number 1))
        )
        
        (gimp-image-undo-group-end image)
        
        (if (= playback TRUE)
            (begin
                (plug-in-animationplay FALSE image layer)
            )
        )
        
    )
        
        (gimp-displays-flush)
        
)


(script-fu-register
    "script-fu-psychedelic-anim"
    "<Toolbox>/Script-Fu2/Animation/Psychedelic anim" 
    "Creates psychedelic animation of an image. \nfile:pyschedelic-anim.scm"
    "SteveMi"
    "Unrestricted"
    "2015"
    "RGB* GRAY*"
    SF-IMAGE        "Image"     0
    SF-DRAWABLE     "Layer"     0
    SF-ADJUSTMENT   "Number of layers to add"   '(15 1 999 1 10 0 0)
    SF-ADJUSTMENT   "Amount of Blur"            '(48 2 300 1 10 0 0)
    SF-ADJUSTMENT   "Number of Colours"         '(15 2 255 1 10 0 0)
    SF-ADJUSTMENT   "Amount to Sharpen"         '(99 0 99 1 10 0 0)
    SF-TOGGLE       "Open layers in gimp playback"      FALSE
)

;end of script