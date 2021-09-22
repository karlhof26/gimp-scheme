; Auto colorize image into random number of colors of random hues
; author: Tin Tran
; date: 2015

; 
; last modified/tested by Tin Tran
; 02/15/2014 on GIMP-2.8.10
; 
; 
; 21/07/2020  - fixed to work on Gimp 2.10.20
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================
; Original information 
; 
; Copyright (C) 2020 Tin Tran
;==============================================================

(define (script-fu-auto-colorize-c image layer 
              hatches
         )
    
    (let* 
          (
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
            (R 0.299)
            (G 0.587)
            (B 0.114)
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
            (try 0)
            (tries 10) ; was 1000
            (counter)
          )
        ;(gimp-image-undo-disable image); DN = NO UNDO
        (gimp-image-undo-group-start image)                   ;undo-group in one step
        ;convert to indexed
        (set! image-width (car (gimp-image-width image)))
        (set! image-height (car (gimp-image-height image)))
        (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE TRUE "unused palette name") ; was F F
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
        (gimp-context-set-sample-threshold 0)
        (while (> y 0) ;; was 0
              ;do work here
              (gimp-message "main loop")
              (gimp-message (number->string y))
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
              ;(set! floating (car (gimp-layer-new image image-width image-height
              ;                 RGBA-IMAGE "Colorize" 100 LAYER-MODE-NORMAL)))  ;creates layer
              ;	;insert above current layer
              ;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
              ;(gimp-image-insert-layer image floating 0 0)
              ;set that layer to be active layer
              ;(gimp-image-set-active-layer image floating)
              
              ;(set! hue (rand 360))
              ;(gimp-colorize floating hue 100 0)
              ;sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
              (set! l-original (sqrt (+ (pow (* red R) 2) (pow (* green G) 2)  (pow (* blue B) 2))) )
              (set! difference 10)
              
              ;just randomly pick a color until we find a color of similar luminance
              ;absolutely not the ideal way of getting a color
              (set! try 255)
              (set! counter tries)
            (while (> difference 1.5) ; was 1
                (if (= counter 0)
                    (begin
                        (set! try (- try 1))
                        (if (< try 0)
                            (begin
                                (set! try 255) ;retry from 255
                            )
                            (begin)
                        )
                        (set! counter tries)
                    )
                    (begin
                        
                    )
                )
                (if (< l-original 56.8)
                    (begin
                        (set! r (rand 255))
                        (set! g (rand 255))
                        (set! b try)        ;try blueish
                    )
                    (begin
                        (if (> l-original (* 56.8 2))
                            (begin
                                (set! r (- (rand 256) 1))
                                (set! g try)       ;try greenish
                                (set! b (rand 255))
                            )
                            (begin
                                (set! r try)       ;try red
                                (set! g (rand 255))      
                                (set! b (rand 255))
                            )
                        )
                    )
                )
                (set! l-new (sqrt(+ (pow(* r R) 2) (pow (* g G) 2)  (pow (* b B) 2))))
                (set! difference (abs (- l-new l-original)))
                (gimp-message (number->string difference))
                
                (set! counter (- counter 1))
            )
            (gimp-message "out of the counter one")
            
                ;(script-fu-colorize image floating (list r g b) 100)
                (gimp-image-set-active-layer image layer)
                (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
                
                (gimp-image-set-active-layer image floating)
                (gimp-context-set-foreground (list r g b))
                (gimp-drawable-edit-fill floating FILL-FOREGROUND)
              
              
            (if (>= y 1) ;if y is still valid we set colors to the next colors
                (begin
                    (set! colors (cdddr colors))
                )
                (begin ;else
                    (gimp-message "inside else line150 end reached")
                )
            )
            
            ;loop control
            (set! y (- y 1))
            (gimp-message "end main loop")
        );end of while 
        
        (gimp-selection-none image)
        ;(gimp-image-undo-enable image) ;DN = NO UNDO
        (gimp-message "Good finish OK")
        (gimp-image-undo-group-end image)                     ;undo group in one step
        (gimp-displays-flush)
    )
    
    
    
) ;end of define

(script-fu-register
    "script-fu-auto-colorize-c"         ;function name
    "<Image>/Script-Fu2/Create from Image/Auto Colorize Flavor C"    ;menu register
    "Randomly colorize image with specified number of colors. \nfile:AutoColorize_FlavorC_3_02.scm"       ;description
    "Tin Tran"                          ;author name
    "copyright info and description"    ;copyright info or description
    "2015"                          ;date
    "RGB*, GRAY*"                   ;mode
    SF-IMAGE      "Image"       0
    SF-DRAWABLE   "Layer"       0
    SF-ADJUSTMENT "Number of colors"    '(5 2 255 1 10 0 0)
    
)

;end of script