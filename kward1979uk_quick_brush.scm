; GIMP script- kward1979uk_quick_brush
; version 1.0 2011.01.31
; Copyright (c) 2022 Karl Ward
; 
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details. 




(define (brush-creator image draw name filename spacing location)
    (let* (
            (drawable 0)
            (selection-bounds 0)
            (sx1 0)
            (sy1 0)
            (sx2 0)
            (sy2 0)
            (swidth 0)
            (sheight 0)
            (newimage 0)
            (newlayer 0)
            (active 0)
            (filename2 0)
        )
        (gimp-image-flatten image)
        (set! drawable (gimp-image-get-active-drawable image))
        (if (= 1 (car (gimp-selection-is-empty image)))
            (gimp-selection-all image)
        )
        (gimp-displays-flush)
        (gimp-edit-copy (car drawable) )
        (set! selection-bounds (gimp-selection-bounds image))
        (set! sx1 (cadr selection-bounds))
        (set! sy1 (caddr selection-bounds))
        (set! sx2 (cadr (cddr selection-bounds)))
        (set! sy2 (caddr (cddr selection-bounds)))
        (set! swidth  (- sx2 sx1))
        (set! sheight (- sy2 sy1))
        (set! newimage (gimp-image-new swidth sheight 0))
        (set! newlayer (gimp-layer-new (car newimage) swidth sheight 1 "newlayer" 100 0))
        (gimp-image-insert-layer (car newimage) (car newlayer) 0 0)
        
        (gimp-drawable-fill (car newlayer) 3)
        (gimp-edit-paste (car newlayer) 0 )
        
        
        (gimp-image-flatten (car newimage))
        (set! active(gimp-image-get-active-drawable (car newimage)))
        (gimp-desaturate (car active))
        (gimp-image-convert-grayscale (car newimage))
        (gimp-displays-flush)
        (gimp-selection-all (car newimage))
        (set! filename2 (string-append location "/" filename ".gbr"))
        (file-gbr-save 1 (car newimage) (car active) filename2 name spacing name)
    )
)

(script-fu-register "brush-creator"
    "<Image>/Script-Fu2/Brush/Quick Brush..."
    "Speeds up layer flatten, desaturate and convert to greyscale. \nfile:kward1979uk_quick_brush.scm"
    "Karl Ward"
    "Karl Ward"
    "Oct 2005"
    ""
    
    SF-IMAGE      "SF-IMAGE" 0
    SF-DRAWABLE   "SF-DRAWABLE" 0
    SF-STRING     "brush name" "name"
    SF-STRING     "File name" "filename"
    SF-ADJUSTMENT "spacing"         '(25 0 1000 1 1 1 0)
    SF-DIRNAME    "SAVE TO FOLDER" ""
)

;end of script