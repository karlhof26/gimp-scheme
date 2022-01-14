; Psychedelic mk2 script 1.0 by  
; Created by 
; 
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
;



(define (script-fu-psychedelic-mk2 image layer layers-added blur posterize sharpen playback)
    
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
            (gimp-posterize (vector-ref (cadr (gimp-image-get-layers image)) number) posterize)
            (plug-in-sharpen 1 image (vector-ref (cadr (gimp-image-get-layers image)) number) sharpen)
            (gimp-levels (vector-ref (cadr (gimp-image-get-layers image)) number) 0 0 160 1 0 255)
            
            (if (= number half-way) (set! option 1))
            
            (case option
                ((0)    (set! up-down (+ up-down 1)))
                ((1)    (set! up-down (- up-down 1)))
            )
            
            (set! number (- number 1))
        )
        
        
        
        (gimp-image-undo-group-end image)
        
        (if (= playback TRUE) (begin (plug-in-animationplay FALSE image layer)))
        
    )
    
    (gimp-displays-flush)
    
)


(script-fu-register
    "script-fu-psychedelic-mk2"
    "<Image>/Script-Fu2/Animation/Psychedelic mk2"
    "Creates psychedelic animation of an image. \nfile:psychedelic-mk2.scm"
    "SteveMi"
    "Unrestricted"
    "2015"
    "RGB* GRAY*"
    SF-IMAGE    "Image"         0
    SF-DRAWABLE "Layer"         0
    SF-ADJUSTMENT   "Number of layers to add"       '(15 1 999 1 10 0 0)
    SF-ADJUSTMENT   "Amount of Blur"                '(48 2 300 1 10 0 0)
    SF-ADJUSTMENT   "Number of Colours"             '(15 2 255 1 10 0 0)
    SF-ADJUSTMENT   "Amount to Sharpen"             '(99 0 99 1 10 0 0)
    SF-TOGGLE       "Open layers in gimp playback"  FALSE
)

;end of script