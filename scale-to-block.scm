; GPLv3

; scale-to-block V1.0
; 
; This script scales an image to a given size then makes the canvas a square with the image centered.
;
; Created by Chris Kent
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
; ------------
;| Change Log |
; ------------ 
; V0.90 - Initial Beta Release 

(define (scale-to-block image drawable size)
    (let*
        (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
        )
        (gimp-image-undo-group-start image)
        (if (> width height)
            (gimp-image-scale image size (* height (/ size width)))
            (gimp-image-scale image (* width (/ size height)) size)
        )
        (gimp-displays-flush)
        
        
        (gimp-image-resize image size size
            (/ (- size (car (gimp-image-width image))) 2)
            (/ (- size (car (gimp-image-height image))) 2)
        )
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-context-set-background '( 0 0 0))
        (gimp-image-flatten image)
        
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        
    )
)

(script-fu-register "scale-to-block"
    "<Image>/Script-Fu3/MTools/Scale To Block"
    "Scales an image to a given size then makes the canvas a square with the image centered. \nfile:scale-to-block.scm"
    "Chris Kent" 
    "WireBear.com" 
    "November 2009"
    "RGB* GRAY*" 
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-VALUE "Block Size" "32"
) 

; end of script