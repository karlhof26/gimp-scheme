; Auto colorize image into random number of colors of random hues
; author: Tin Tran 
; date: 2015
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
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function

(define (script-fu-auto-colorize-a 
              image
              layer 
              hatches
         )
    
        (let* (
                (color-map 0)
                (colors 0)
                (red 0)
                (green 0)
                (blue 0)
                (y 0)
                (hue 0)
                (floating 0)
              )
            ;(gimp-image-undo-disable image); DN = NO UNDO
            (gimp-image-undo-group-start image)  ;undo-group in one step
            ;            (gimp-message "one")
            ;convert to indexed
            (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE FALSE "unused palette name")
            ;grabs color map
            (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
            
            (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
            
            (set! y hatches)  ;loop hatches number of times
            (srand (car (gettimeofday)))
            (gimp-context-set-sample-threshold 0.01) ; was 0
            ;;       (gimp-message "two")
            (while (> y 0)
                ;do work here
                ;;       (gimp-message "three")
                (set! red (car colors))
                (set! green (cadr colors)) 
                (set! blue (caddr colors))
                ;select each color
                (gimp-image-set-active-layer image layer)
                (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
                ;(set! hue (rand 360))
                ;(gimp-colorize layer hue 100 0)
                (gimp-edit-copy layer)
                (set! floating (car(gimp-edit-paste layer TRUE)))
                (gimp-floating-sel-to-layer floating)
                (gimp-image-set-active-layer image floating)
                (set! hue (rand 359)) ; was rand 360
                (gimp-drawable-colorize-hsl floating hue 100 0)
                ;(script-fu-colorize image floating (list (rand 255) (rand 255) (rand 255)) 100)
                (if (> y 1) ;if y is still valid we set colors to the next colors
                    (begin
                        (set! colors (cdddr colors))
                    )
                    (begin ;else
                    )
                )
                
                ;loop control
                (set! y (- y 1))
            );end of while
            ;           (gimp-message "four")
            (gimp-selection-none image)	
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            (gimp-image-undo-group-end image)                     ;undo group in one step
            (gimp-displays-flush)
     )
) ;end of define

(script-fu-register
    "script-fu-auto-colorize-a"         ;function name
    "<Image>/Script-Fu2/Create from Image/Auto Colorize Flavor A"    ;menu register
    "Randomly colorize image with specified number of colors. \nfile:AutoColorize_FlavorA_1_02.scm"       ;description
    "Tin Tran"                          ;author name
    "copyright info and description"         ;copyright info or description
    "2015"                               ;date
    "RGB*, GRAY*"                        ;mode
    SF-IMAGE      "Image"  0                   
    SF-DRAWABLE   "Layer"  0
    SF-ADJUSTMENT "Number of colors"  '(5 2 255 1 10 0 0)
)

;------------------------
;end of script