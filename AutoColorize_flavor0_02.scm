; GPLv3
;; This script was tested with Gimp 2.10.22
;;
;; New versions will be distributed from <http://github.com/> only
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses>.
;;
; ;Define the function
; Auto colorize image into random number of colors of random hues
; author: Tin Tran
; date: 2015


(define (script-fu-auto-colorize-flavor0 simage slayer 
              hatches
              rgb
              flatten
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
            ;(B (/ 18.0 255))
            ;(R (/ 54.0 255))
            ;(G (/ 182.0 255))
            ;wikipedia
            (Bl 0.0722)
            (Re 0.2126)
            (Gr 0.7152)
            ;how my camera sees black and white
            ;(B (/ 147 479))
            ;(R (/ 138 479))
            ;(G (/ 194 479))
            (r 0)          ;randomly generated r g b values
            (g 0)
            (b 0)
            (l-original 0) ;luminance original
            (l-new 0)
            (red 0)
            (green 0)
            (blue 0)
            (y 0)
            (hue)
            (floating)
            (difference)
            (image)
            (layer)
            (try 2)
            (tries 256)
            (counter 1)
            (loopbreak 1)
          )
          
          
          ;(gimp-image-undo-disable image); DN = NO UNDO
          (gimp-image-undo-group-start simage)                   ;undo-group in one step
          ;convert to indexed
          (set! image-width (car (gimp-image-width simage)))
          (set! image-height (car (gimp-image-height simage)))
          
          
          ;creates new image instead of working on existing one
          (gimp-selection-all simage)
          (gimp-edit-copy-visible simage)
          (gimp-edit-copy slayer)
          (set! image (car (gimp-edit-paste-as-new-image)))
          (set! layer (car (gimp-image-get-active-layer image)))
          ;(gimp-message "ok to here 69")
          (set! floating (car (gimp-layer-new image image-width image-height
                               RGBA-IMAGE "Colorize" 100 LAYER-MODE-NORMAL-LEGACY)))  ;creates layer
                ;insert above current layer
                ;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
                (gimp-image-insert-layer image floating 0 0)   ; was 0 0
                (gimp-drawable-edit-fill floating FILL-TRANSPARENT)
                
          (gimp-display-new image)
          (gimp-displays-flush)
          ;(set! image (car (gimp-image-new width height RGB)))      ;creates new image
          
          
          
          (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE FALSE "unused palette name")
          ;grabs color map
          (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
          
          (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
          
          
          (set! y hatches) ;loop hatches number of times
          ;; (srand (car (gettimeofday)))
          (srand (realtime))  
          ;(gimp-message "time ok")
          (gimp-context-set-sample-threshold-int 5)
          (gimp-context-set-sample-criterion 0)
          
          (while (> y 0)
                ;do work here
                (set! red (car colors))
                (set! green (cadr colors))
                (set! blue (caddr colors))
                ;select each color
                ;(gimp-message (number->string r))
                ;(gimp-message (number->string red))
                ;(gimp-message (number->string b))
                ;(gimp-message (number->string blue))
                
                (gimp-image-set-active-layer image layer)
                (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue)) ; was layer
                ;(set! hue (rand 360))
                ;(gimp-colorize layer hue 100 0)
                ;(gimp-message "color selected")
                
                ;(gimp-edit-copy layer)
                ;(set! floating (car(gimp-edit-paste layer TRUE)))
                ;(gimp-floating-sel-to-layer floating)
                ;(gimp-image-set-active-layer image floating)
                ;;;(set! floating (car (gimp-layer-new image image-width image-height
                ;;;               RGBA-IMAGE "Colorize" 100 LAYER-MODE-NORMAL-LEGACY)))  ;creates layer
                ;insert above current layer
                ;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
                ;;;(gimp-image-insert-layer image floating 0 1)   ; was 0 0
                ;;;(gimp-drawable-edit-fill floating FILL-TRANSPARENT)
                ;set that layer to be active layer
                ;;;(gimp-image-set-active-layer image floating)
                ;;;(gimp-edit-copy layer)
                ;;;(set! floating (car (gimp-edit-paste layer TRUE)))
                ;;;(gimp-floating-sel-to-layer floating)
                ;;;(gimp-image-set-active-layer image floating)
                
                (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue)) ; was layer
                
                ;;;(gimp-displays-flush)
                ;;;(gimp-message "quit now")
                ;;;(quit)
                
                ;(set! hue (rand 360))
                ;(gimp-colorize floating hue 100 0)
                ;sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
                ;(set! l-original (sqrt(+ (pow(* red R) 2) (pow (* green G) 2)  (pow (* blue B) 2))))
                (set! l-original (+ (* red Re) (* green Gr) (* blue Bl)))
                (set! difference 10)
                
                ;just randomly pick a color until we find a color of similar luminance
                ;absolutely not the ideal way of getting a color
                
                (set! loopbreak 1)
                (while (and (> difference 1) (< loopbreak 550))
                    
                    (if (< l-original 10)
                        (begin
                            (set! r (rand 21))
                            (set! g (rand 21))
                            (set! b (rand 21))
                        )
                        (begin
                            (if (> l-original 245)
                                (begin
                                    (set! r (+ (rand 20) 236))
                                    (set! g (+ (rand 20) 236))
                                    (set! b (+ (rand 20) 236))
                                    
                                )
                                (begin
                                    (set! r (rand 255)) ; was 256
                                    (set! g (rand 255)) ; was 256
                                    (set! b (rand 255)) ; was 256
                                )
                            )
                        )
                    )
                    (if (= rgb 1)
                        (begin
                            ;;(gimp-message "rgb=1")
                            (if (< l-original 85)
                                (begin
                                    (while (or (< b r) (< b g))
                                        (if (< l-original 10)
                                            (begin
                                                (set! r (rand 21))
                                                (set! g (rand 21))
                                                (set! b (rand 21))
                                            )
                                            (begin
                                                (if (> l-original 245)
                                                    (begin
                                                        (set! r (+ (rand 20) 236))
                                                        (set! g (+ (rand 20) 236))
                                                        (set! b (+ (rand 20) 236))
                                                        
                                                    )
                                                    (begin
                                                        (set! r (rand 255)) ;was 256
                                                        (set! g (rand 255)) ; was 256
                                                        (set! b (rand 255)) ; was 256
                                                    )
                                                )
                                                
                                            )
                                        )
                                        
                                    )
                                )
                                (begin
                                    (if (> l-original (* 85 2))
                                        (begin
                                            (while (or (< g r) (< g b))
                                                (if (< l-original 10)
                                                    (begin
                                                        (set! r (rand 21))
                                                        (set! g (rand 21))
                                                        (set! b (rand 21))
                                                    )
                                                    (begin
                                                        (if (> l-original 245)
                                                            (begin
                                                                (set! r (+ (rand 20) 236))
                                                                (set! g (+ (rand 20) 236))
                                                                (set! b (+ (rand 20) 236))
                                                                
                                                            )
                                                            (begin
                                                                (set! r (rand 255)) ; was 256
                                                                (set! g (rand 255)) ; was 256
                                                                (set! b (rand 255)) ; was 256
                                                            )
                                                        )
                                                        
                                                    )
                                                )
                                            )
                                        )
                                        (begin
                                            (while(or (< r g) (< r b))
                                                
                                                (if (< l-original 10)
                                                    (begin
                                                        (set! r (rand 21))
                                                        (set! g (rand 21))
                                                        (set! b (rand 21))
                                                    )
                                                    (begin
                                                        (if (> l-original 245)
                                                            (begin
                                                                (set! r (+ (rand 20) 236))
                                                                (set! g (+ (rand 20) 236))
                                                                (set! b (+ (rand 20) 236))
                                                                
                                                            )
                                                            (begin
                                                                (set! r (rand 255)) ; was 256
                                                                (set! g (rand 255)) ; was 256
                                                                (set! b (rand 255)) ; was 256
                                                            )
                                                        )
                                                        
                                                        
                                                        
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (begin
                            ;;(gimp-message "rgb=0")
                            ;;(gimp-message "inside here")
                        )
                        
                    )
                    
                    (set! loopbreak (+ loopbreak 1))
                    ;(set! l-new (sqrt(+ (pow(* r R) 2) (pow (* g G) 2)  (pow (* b B) 2))))
                    (set! l-new (+ (* r Re) (* g Gr) (* b Bl)))
                    (set! difference (abs (- l-new l-original)))
                )
                
                ;;(gimp-message "ready to color")
                ;(gimp-message (number->string difference))
                ;(gimp-message (number->string r))
                ;(gimp-message (number->string red))
                ;(script-fu-colorize image floating (list r g b) 100)
                
                ;(gimp-message "ready to color2")
                (gimp-image-set-active-layer image layer)
                (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
                (gimp-image-set-active-layer image floating)
                ;(gimp-context-set-foreground '(123 0 240))
                (gimp-context-set-foreground (list r g b))
                (gimp-drawable-edit-fill floating FILL-FOREGROUND) ;; was image-edit-fill
                ;;;(gimp-selection-none image)
                
                (if (> y 1) ;if y is still valid we set colors to the next colors
                    (begin
                        (set! colors (cdddr colors))
                    )
                )
                
                ;loop control
                (set! y (- y 1))
                
                ;(gimp-message "flushing and while looping")
                ;(gimp-displays-flush)
          );end of while 
            (gimp-selection-none image)
            (if (= flatten TRUE)
                (begin
                    (gimp-image-flatten image)
                )
                (begin)
            )
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            (gimp-image-undo-group-end simage)                     ;undo group in one step
            (gimp-displays-flush)
            (gc)
        )
    
    
    
) ;end of define

(script-fu-register
    "script-fu-auto-colorize-flavor0"                                           ;function name
    "<Image>/Script-Fu2/Create from Image/Auto Colorize flavour0..."    ;menu register
    "Randomly colorize image with specified number of colors."       ;description
    "Tin Tran"                          ;author name
    "copyright info and description"         ;copyright info or description
    "2015"                          ;date
    "RGB*, GRAY*"                        ;mode
    SF-IMAGE        "Image"   0                   
    SF-DRAWABLE     "Layer"   0
    SF-ADJUSTMENT   "Number of colors"       '(5 2 255 1 10 0 0)
    SF-OPTION       "Colorize with"          '("Random colors" "RGB variations")
    SF-TOGGLE       "Flatten newly created image"      FALSE
    
)
    
    ;;-----------------------------