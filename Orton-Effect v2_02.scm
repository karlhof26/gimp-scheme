;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details. 
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
;
(define (script-fu-orton-effect-aa inImage inLayer inBlur inOpacity inKeepOriginal)
    
    (gimp-image-undo-group-start inImage)
    
    (define (copy-and-add-layer orig-layer)
        (let ((layer (car (gimp-layer-copy orig-layer TRUE))))
            (gimp-image-insert-layer inImage layer 0 -1)
            layer
        )
    )
    
    (define (create-light-layer orig-layer)
        (let ((layer (copy-and-add-layer orig-layer)))
            (gimp-layer-set-mode layer LAYER-MODE-SCREEN-LEGACY)
            (car (gimp-image-merge-down inImage layer CLIP-TO-BOTTOM-LAYER))
        )
    )
    
    (let* (
            (layer (create-light-layer
                (if (= inKeepOriginal TRUE)
                (copy-and-add-layer inLayer)
                inLayer)
                ))
            (layer2 (copy-and-add-layer layer))
          )
        (if (> inBlur 0)
            (plug-in-gauss 1 inImage layer2 inBlur inBlur 0)
        )
        (gimp-layer-set-mode layer2 LAYER-MODE-MULTIPLY-LEGACY)
        (gimp-layer-set-opacity layer2 inOpacity)
    )
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
)
;

(script-fu-register "script-fu-orton-effect-aa"
    "<Toolbox>/Script-Fu2/Artistic/Orton Effect v2"
    "Imitates Orton effect by creating two \ 
    lighter layers from the original - \
    one blurred and one sharp, and mixing them.\
    Description of this method: http://pcin.net/update/2006/11/01/the-orton-effect-digital-photography-tip-of-the-week \
    \nfile:Orton-Effect v2_02.scm"
    "Julia Jomantaite <julia.jomantaite@gmail.com>"
    "Julia Jomantaite"
    "20.04.2008"
    "RGB* GRAY*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Layer"         0
    SF-ADJUSTMENT   "Blur radius"       '(10.0 0.0 99.0 1.0 5 1 0)
    SF-ADJUSTMENT   "Opacity"           '(100.0 0.0 100.0 1.0 5 1 0)
    SF-TOGGLE       "Keep the original layer"   TRUE
)

;end of script