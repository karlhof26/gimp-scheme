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
(define (square image drawable 500yn scaleamt expandyn)
    (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (newWidth width)
            (newHeight height)
            (layer (vector-ref (cadr (gimp-image-get-layers image)) 0))
            (halfheight 0)
            (halfwidth 0)
          )
        (gimp-image-undo-group-start image)
        (gimp-context-push)
        
        (if (= expandyn 0)
            (begin
                (plug-in-autocrop 1 image drawable)    
                (if (< width height)
                    (set! newWidth height)
                )
                (if (> width height)
                    (set! newHeight width)
                )
                
                (gimp-image-resize image newWidth newHeight (/ (- newWidth width) 2) (/ (- newHeight height) 2))
                (gimp-layer-resize-to-image-size layer)
            )
        )
        
        (if (= expandyn 1)
            (begin
                
                (if (< width height)
                    (begin
                        ;(gimp-message "portrait image")
                        (set! newWidth width)
                        (set! newHeight width)
                        ; this is a portrait image
                        ; use height to determine squaring
                        ; calc height / 2
                        (set! halfheight (/ height 2))
                        ;calc width / 2
                        (set! halfwidth (/ width 2))
                        ; select from 1/2 height-width/2 to 1/2height + width/2 x by width wide
                        
                        ;(gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 (- halfheight halfwidth) width (+ halfheight halfwidth))
                        (gimp-image-crop image width width 0 (- halfheight halfwidth))
                    )
                    
                    
                )
                (if (> width height)
                    (begin
                        ;(gimp-message "landscape image")
                        (set! newWidth height)
                        (set! newHeight height)
                        ; this is a portrait image
                        ; use height to determine squaring
                        ; calc height / 2
                        (set! halfheight (/ height 2))
                        ;calc width / 2
                        (set! halfwidth (/ width 2))
                        ; select from 1/2 height-width/2 to 1/2height + width/2 x by width wide
                        
                        ;(gimp-image-select-rectangle image CHANNEL-OP-REPLACE (- halfwidth halfheight) 0 height (+ halfwidth halfheight) )
                        
                        (gimp-image-crop image height height (- halfwidth halfheight) 0)
                    )
                )
                
                
                
            )
        )
        
        (if (= 500yn 1)
            (begin
                (if (and (> newWidth 500) (> newHeight 500))
                    (gimp-image-scale image 500 500) 
                )
            )
        )
        
        (if (= 500yn 2)
            (begin
                
                    (gimp-image-scale image scaleamt scaleamt) 
                
            )
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
    SF-IMAGE    "Input Image"               0
    SF-DRAWABLE "Input Drawable"            0
    SF-OPTION   "Scale option"                  '("None" "500x500px" "User selected")
    SF-ADJUSTMENT   "User scale amount px"      '(340 5 5000 10 100 0 SF-SPINNER)
    SF-OPTION "Expand image or Lose edges"      '("Expand" "Lose edges")
)

; end of script