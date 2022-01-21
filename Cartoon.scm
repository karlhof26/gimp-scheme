; Cartoon script 1.0 by daoo 
; Created by daoo    
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

(define (script-fu-daoocartoon img drawable blurx blury)
    (gimp-image-undo-group-start img)
    ;(gimp-edit-copy drawable)
    
    (let* (
            (theFloat (car (gimp-layer-copy drawable TRUE)))
            (theBlur (car (gimp-layer-copy drawable TRUE)))
        ) 
        (gimp-image-insert-layer img theBlur 0 -1)
        (gimp-image-insert-layer img theFloat 0 -1)
        (gimp-item-set-name theFloat "theFloat")
        (gimp-item-set-name theBlur "Base-layer")
        
        (plug-in-gauss-iir2 1 img theFloat blurx blury)
        ;(gimp-drawable-invert theFloat TRUE)
        (gimp-layer-set-mode theFloat LAYER-MODE-SUBTRACT) ; was DARKEN-ONLY-LEGACY
        (gimp-displays-flush)
        (set! theBlur (car (gimp-image-merge-down img theFloat EXPAND-AS-NECESSARY)))
        (gimp-drawable-levels theBlur HISTOGRAM-VALUE 0.0 0.50 TRUE 1.85 0.0 1.0 TRUE)
    )
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc)
)

(script-fu-register "script-fu-daoocartoon"
    "Cartoon daoo..."
    "Makes your image look like a cartoon. Blur and Colorize \nfile:Cartoon.scm"
    "daoo"
    "2006, daoo creation"
    "2006"
    "RGBA RGB GRAY"
    SF-IMAGE "img"               0
    SF-DRAWABLE "drawable"       0
    SF-ADJUSTMENT "Blur Horizontal"   '(10 0 100 1 10 1 0)
    SF-ADJUSTMENT "Blur Vertical"     '(10 0 100 1 10 1 0)
)

(script-fu-menu-register "script-fu-daoocartoon" "<Toolbox>/Script-Fu/Decor/Photo Effects/Artist")

(script-fu-menu-register "script-fu-daoocartoon" "<Toolbox>/Script-Fu3/Comics-o-matic")

; end of script 