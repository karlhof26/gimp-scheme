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

(define (script-fu-auto-colorize-g image layer 
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
            (B (/ 18.0 255))
            (R (/ 54.0 255))
            (G (/ 182.0 255))
            (r)          ;randomly generated r g b values
            (g)
            (b)
            (l-original) ;luminance original
            (l-new)
            (red 0)
            (green 0)
            (blue 0)
            (y 0)
            (hue)
            (floating)
            (difference)
            (try)
            (tries 30)
            (counter)
            (randomlayer (car (gimp-layer-copy layer FALSE)))
            
            
            (bigcounter 0)
        )
            ;(gimp-image-undo-disable image); DN = NO UNDO
            (gimp-image-undo-group-start image)                   ;undo-group in one step
            ;convert to indexed
            (set! image-width (car (gimp-image-width image)))
            (set! image-height (car (gimp-image-height image)))
            (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE FALSE "unused palette name")
            ;grabs color map
            (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
            
            (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
            (gimp-image-insert-layer image randomlayer 0 -1)
            (gimp-item-set-name randomlayer "random colour layer")
            
            
            (set! y hatches) ;loop hatches number of times
            (srand (car (gettimeofday)))
            (gimp-context-set-sample-threshold 0)
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
                (set! floating (car (gimp-layer-new image image-width image-height
                           RGBA-IMAGE "Colorize" 100 LAYER-MODE-NORMAL)))  ;creates layer
                ;insert above current layer
                ;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
                (gimp-image-insert-layer image floating 0 0)	 	  
                ;set that layer to be active layer
                (gimp-image-set-active-layer image floating)
                
                ;(set! hue (rand 360))
                ;(gimp-colorize floating hue 100 0)
                ;sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
                ;(set! l-original (sqrt(+ (pow(* red R) 2) (pow (* green G) 2)  (pow (* blue B) 2))))
                (set! l-original (+ (* red R) (* green G)  (* blue B)))
                (set! difference 10)
                
                ;just randomly pick a color until we find a color of similar luminance
                ;absolutely not the ideal way of getting a color
                (set! try 255)
                (set! counter tries)
                
                (set! bigcounter 4000)
                (if (= y 1)
                    (begin
                        ;(gimp-message "LAST ONE")
                    )
                )
                
                (while (and (> difference 1.5) (> bigcounter 1))
                    (if (= counter 0)
                        (begin
                            (set! try (- try (rand 5)))
                            (if (< try 1) ; was try<0
                                (begin
                                    ;(gimp-message "try below 0 - reset")
                                    (set! try 255) ;retry from 255
                                )
                                (begin
                                    (if (> try 40)
                                        (begin
                                            ;(gimp-message "still gr 40")
                                            ;(gimp-message (number->string try))
                                        )
                                    )
                                )
                            )
                            (set! counter tries)
                        )
                        (begin
                            
                        )
                    )
                    (if (< l-original 75) ; was 85
                        (begin
                            (set! r (- (rand 256) 1))
                            (set! g try)                            ;try green was red
                            (set! b (- (rand 256) 1))        
                        )
                        (begin
                            (if (> l-original (* 85 2)) ; was 85
                                (begin
                                    (set! r try)                    ; try red was green
                                    (set! g (- (rand 256) 1))       ;
                                    (set! b (- (rand 256) 1))
                                )
                                (begin
                                    (set! r (- (rand 256) 1))      
                                    (set! g (- (rand 256) 1))      
                                    (set! b try)                    ; try blue was red
                                )
                            )
                        )
                    )
                    ;(set! l-new (sqrt(+ (pow(* r R) 2) (pow (* g G) 2)  (pow (* b B) 2))))
                    (set! l-new (+ (* r R) (* g G)  (* b B)))
                    (set! difference (abs (- l-new l-original)))
                    
                    (set! counter (- counter 1))
                    (set! bigcounter (- bigcounter 1))
                    (gimp-progress-update (/ bigcounter 4000))
                )
                
                ;(gimp-message (number->string r)) 
                ;(gimp-message (number->string g))
                ;(gimp-message (number->string b))
                
                (gimp-image-set-active-layer image floating)
                
                (gimp-context-set-foreground (list r g b))
                (gimp-edit-fill floating FILL-FOREGROUND)
                
                
                (if (> y 1) ;if y is still valid we set colors to the next colors
                    (begin
                        (set! colors (cdddr colors))
                    )
                    (begin ;else
                    )
                )
                
                ;loop control
                (set! y (- y 1))
                (gimp-displays-flush)
            );end of while 
            
            
            (gimp-selection-none image)
            
            (gimp-image-set-active-layer image randomlayer)
            ;(gimp-drawable-colorize-hsl randomlayer (rand 359) 100 1)
            (plug-in-alienmap2 1 image randomlayer 8 40 1 0 1 0 0 TRUE TRUE TRUE)
            ;(gimp-message "alien")
            ;(plug-in-alienmap2 1 image randomlayer (rand 6) (rand 20) (rand 6) (rand 20) (rand 6) (rand 20) 0 TRUE TRUE TRUE)
            ;(gimp-drawable-colorize-hsl floating (rand 359) 100 1) 
            
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            (gimp-image-undo-group-end image)                     ;undo group in one step
            (gimp-displays-flush)
    )
) ;end of define

(script-fu-register
    "script-fu-auto-colorize-g"         ;function name
    "<Image>/Script-Fu2/Create from Image/Auto Colorize Flavor G"    ;menu register
    "Randomly colorize image with specified number of colors. Also creates a second random color layer red priority. This version has green/blue priority so it may generate different effects. Also generates a separate layer for each color. Hide layers for interesting combinations.\nfile:AutoColorize_FlavorG_ver3_02.scm"       ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2015"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image"                   0      
    SF-DRAWABLE   "Layer"                   0
    SF-ADJUSTMENT "Number of colors"        '(5 2 50 1 10 0 0) ; was 255 at max
)

; end of file