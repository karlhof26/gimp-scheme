; GPLv3
; Licence GNU/GPL
;;
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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;
;
;
(define (square image drawable)
    (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (newWidth width)
            (newHeight height)
            (layer (vector-ref (cadr (gimp-image-get-layers image)) 0))
          )
        (gimp-image-undo-group-start image)
        (gimp-context-push)
        (plug-in-autocrop 1 image drawable)    
        (if (< width height)
            (set! newWidth height)
        )
        (if (> width height)
            (set! newHeight width)
        )
        
        (gimp-image-resize image newWidth newHeight (/ (- newWidth width) 2) (/ (- newHeight height) 2))
        (gimp-layer-resize-to-image-size layer)
        
        (if (and (> newWidth 500) (> newHeight 500))
            (gimp-image-scale image 500 500) 
        )
        
        ;(gimp-image-resize-to-layers image)
        
        (gimp-image-clean-all image)
        (gimp-context-pop)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        
        (list newWidth newHeight)
    )
)
(script-fu-register  "square"
    "<Image>/Script-Fu3/MTools/Square"
    "Make image into a square. \nfile:square.scm"
    "Matthew Morrone"
    "GPLv3 2016"
    "12 May 2016"
    "*"
    SF-IMAGE    "Input Image"       0
    SF-DRAWABLE "Input Drawable"    0
)

; end of script