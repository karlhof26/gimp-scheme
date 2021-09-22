; Auto colorize image into random number of colors of random hues
; author: Tin Tran
; date: 2015

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

(define (script-fu-auto-colorize-b image layer 
              hatches
         )
        
    (let* (
            (color-map 0)
            (colors 0)
            (image-width)
            (image-height)
            ;(R 0.2126)   ;constants for calculating luminance
            ;(G 0.7152)
            ;(B 0.0722)
            ;(0.299*R + 0.587*G + 0.114*B)
            ;(R 0.299)
            ;(G 0.587)
            ;(B 0.114)
            ;sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
            (Re 0.299)
            (Gr 0.587)
            (Bl 0.114)
            (r 1)          ;randomly generated r g b values
            (g 2)
            (b 3)
            (l-original 0.1) ;luminance original
            (l-new 0.0)
            (red 0)
            (green 0)
            (blue 0)
            (y 0)
            (hue)
            (floating 0)
            (difference)
            (counter 0)
          )
            ;(gimp-image-undo-disable image); DN = NO UNDO
          (gimp-image-undo-group-start image)                   ;undo-group in one step
          ;convert to indexed
          (set! image-width (car (gimp-image-width image)))
          (set! image-height (car (gimp-image-height image)))
          (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE TRUE "unused palette name") ;; was F F "name"
          ;grabs color map
          (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
          
          (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
          (set! floating (car (gimp-layer-new image image-width image-height
                               RGBA-IMAGE "Colorize" 100 LAYER-MODE-NORMAL)))  ;creates layer
                ;;insert above current layer
                ;;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
          (gimp-image-insert-layer image floating 0 0)    ;;was 0 0      
          (gimp-drawable-edit-fill floating FILL-TRANSPARENT)   ;; added by karlhof26
                ;set that layer to be active layer
                
          (set! y hatches) ;loop hatches number of times
          (srand (car (gettimeofday)))
          (gimp-context-set-sample-threshold-int 5)
          (gimp-context-set-sample-criterion 0)
          
          (while (> y 0)
              ;do work here
              (set! red (car colors))
              (set! green (cadr colors))
              (set! blue (caddr colors))
              ;select each color
              (gimp-image-set-active-layer image layer)
              (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
              ;(set! hue (rand 360))
              ;(gimp-colorize layer hue 100 0)
              
              
              ;(gimp-edit-copy layer)
              ;(set! floating (car(gimp-edit-paste layer TRUE)))
              ;(gimp-floating-sel-to-layer floating)
              ;(gimp-image-set-active-layer image floating)
              
              (gimp-image-set-active-layer image floating)
                
                ;;(set! hue (rand 360))
                ;;(gimp-colorize floating hue 100 0)
                ;;sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
              (set! l-original (sqrt (+ (pow (* red Re) 2) (pow (* green Gr) 2)  (pow (* blue Bl) 2))))
              (set! difference 10)
              (set! counter 0)
              ;just randomly pick a color until we find a color of similar luminance
              ;absolutely not the ideal way of getting a color
              (while (and (> difference 1.5) (< counter 220))  ; was 1 was 1.5
                    (set! r (- (rand 256) 1)) ; was 255
                    (set! g (rand 255))
                    (set! b (rand 255))
                    (set! l-new (sqrt (+ (pow (* r Re) 2) (pow (* g Gr) 2)  (pow (* b Bl) 2))))
                    (set! difference (abs (- l-new l-original)))
                    (gimp-message "Picking....")
                    ;(gimp-message (number->string (rand 10)))
                    (set! counter (+ counter 1))
              )
              
              ;(script-fu-colorize image floating (list r g b) 100)
              ;(gimp-message (number->string difference))
              (gimp-image-set-active-layer image layer)
              (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
              (gimp-image-set-active-layer image floating)
              (gimp-context-set-foreground (list r g b))
              (gimp-drawable-edit-fill floating FILL-FOREGROUND)
              
              
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
        (gimp-selection-none image)
        ;(gimp-image-undo-enable image) ;DN = NO UNDO
        (gimp-image-undo-group-end image)                     ;undo group in one step
        (gimp-displays-flush)
    )
    
    
) ;end of define

(script-fu-register
  "script-fu-auto-colorize-b"         ;function name
  "<Image>/Script-Fu2/Create from Image/Auto Colorize Flavor B"    ;menu register
  "Randomly colorize image with specified number of colors."       ;description
  "Tin Tran"                          ;author name
  "copyright info and description"         ;copyright info or description
  "2015"                          ;date
  "RGB*, GRAY*"                        ;mode
  SF-IMAGE      "Image" 0                   
  SF-DRAWABLE   "Layer" 0
  SF-ADJUSTMENT "Number of colors" '(5 2 255 1 10 0 0)
  
)

;end of script