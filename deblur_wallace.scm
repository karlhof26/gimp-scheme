; GPLv3
; modified by karlhof26
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


(define (script-fu-deblur-wallace image layer 
            blur-radius
            layer-mode
            opacity
          )
          
    (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (active-vectors 0)
            (dup-layer 0)
            (new-from-visible-layer 0)
          )
        ;(gimp-image-undo-disable image); DN = NO UNDO
        (gimp-context-push)
        (gimp-image-undo-group-start image)                   ;undo-group in one step
            
        ;duplicate layer
        (set! dup-layer (car (gimp-layer-copy layer TRUE)))
        (gimp-image-insert-layer image dup-layer 0 0)
        
        ;gausian blur
        (plug-in-gauss 1 image dup-layer blur-radius blur-radius 1)
        
        ;invert
        ;(gimp-invert dup-layer)
        (gimp-drawable-invert dup-layer FALSE)
        
        ;set mode to grain merge
        (gimp-layer-set-mode dup-layer GRAIN-MERGE-MODE)
        
        ;new from visible
        (set! new-from-visible-layer (car (gimp-layer-new-from-visible image image "new-from-visible")))
        (gimp-image-insert-layer image new-from-visible-layer 0 -1)
        ;remove dup-layer
        (gimp-image-remove-layer image dup-layer)
        
        ;set mode
        (gimp-layer-set-mode new-from-visible-layer layer-mode)
        
        ;gimp-layer-set-opacity
        (gimp-layer-set-opacity new-from-visible-layer opacity)
        
        ;(gimp-image-undo-enable image) ;DN = NO UNDO
        (gimp-image-undo-group-end image)                     ;undo group in one step
        (gimp-context-pop)
        (gimp-displays-flush)
    )
) ;end of define

(script-fu-register
    "script-fu-deblur-wallace"              ;function name
    "<Image>/Script-Fu2/Enhance/Deblur (by Wallace)..."    ;menu register
    "Adds a deblur layer to the image. Try Grain-Merge as the mode to deblur \nfile:deblur_wallace.scm"       ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2016"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image"           0                   
    SF-DRAWABLE   "Layer"           0
    SF-ADJUSTMENT "Blur Radius"     '(15 1 500 1 10 0 0)
    SF-ENUM       "Set Layer Mode"  '("LayerModeEffects" "grain-merge-mode") ; "normal-mode"
    SF-ADJUSTMENT "Opacity"         '(50 0 100 1 10 0 0)
)

;end of script