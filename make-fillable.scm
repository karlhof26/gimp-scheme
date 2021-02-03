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
; Rel 0.02 - Updated for GIMP-2.10.22 by karlhof26 (Nov 2020)

(define (comic-fu-make-fillable-k  img
                                  linelayer
                                  alpha-threshold)
    
    
    (let* (
            (tempvar 0)
            (color-layer (car (gimp-layer-copy linelayer 0)))
            (color-layer-position (+ 1 (car (gimp-image-get-layer-position img linelayer))))
          )
        (gimp-image-undo-group-start img)
        (gimp-image-insert-layer img color-layer 0 color-layer-position)
        (gimp-drawable-set-name color-layer (string-append (car (gimp-drawable-get-name linelayer)) " colors"))
        (plug-in-threshold-alpha 1 img color-layer alpha-threshold)
        ;;(plug-in-threshold-alpha 1 img color-layer alpha-threshold)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register "comic-fu-make-fillable-k"
            "Make Fillable"                                     ;menu label
            "Duplicates the current layer and alpha thresholds the duplicate, so it can be easily bucket-filled. Layer must contain some alpha. \nfile:make-fillable.scm"
            "Paul Bonser <pib@paulbonser.com>"
            "Paul Bonser"
            "February 9, 2010"
            "RGB*"
            SF-IMAGE       "Image"              0
            SF-DRAWABLE    "Drawable"           0
            SF-ADJUSTMENT  "Alpha Threshold"    '(220 1 255 1 10 0 0)
)

(script-fu-menu-register "comic-fu-make-fillable-k"
                         "<Toolbox>/Script-Fu2/Effects")

;end of script