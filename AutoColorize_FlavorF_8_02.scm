; Auto colorize image into random number of colors of random hues
; author: Tin Tran
; date: 2015
; spaced out by Karl
(define (script-fu-auto-colorize-f
                simage slayer 
                hatches
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
            (B 0.0722)
            (R 0.2126)
            (G 0.7152)
            ;how my camera sees black and white
            ;(B (/ 147 479))
            ;(R (/ 138 479))
            ;(G (/ 194 479))
            (r 0)          ;randomly generated r g b values
            (g 0)
            (b 0)
            (l-original) ;luminance original
            (l-new)
            (red 0)
            (green 0)
            (blue 0)
            (y 0)
            (hue)
            (floating)
            (difference)
            (image)
            (layer)
            (try)
            (tries 256)
            (counter)
        )
            
            
            ;(gimp-image-undo-disable image); DN = NO UNDO
            ;(gimp-image-undo-group-start image)                   ;undo-group in one step
            ;convert to indexed
            (set! image-width (car (gimp-image-width simage)))
            (set! image-height (car (gimp-image-height simage)))
            
            
            ;creates new image instead of working on existing one
            (gimp-selection-all simage)
            (gimp-edit-copy slayer)
            (set! image (car (gimp-edit-paste-as-new)))
            (set! layer (car(gimp-image-get-active-layer image)))
            (gimp-display-new image)
            ;(set! image (car (gimp-image-new width height RGB)))      ;creates new image
            
            
            
            (gimp-image-convert-indexed image CONVERT-DITHER-FS CONVERT-PALETTE-GENERATE hatches FALSE FALSE "unused palette name")
            ;grabs color map
            (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
            
            (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
            
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
                ;
                (set! hue (rand 359))
                (gimp-drawable-colorize-hsl layer hue 100 5)
                
                
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
                (set! l-original (+ (* red R) (* green G) (* blue B)))
                (set! difference 10)
                
                ;just randomly pick a color until we find a color of similar luminance
                ;absolutely not the ideal way of getting a color
                (while (> difference 1)
                    (if (< l-original 20) ; was 10
                        (begin
                            (set! r (rand 31)) ; was 21 ; makes it different to Flavor E
                            (set! g (rand 41))
                            (set! b (rand 41))
                        )
                        (begin
                            (if (> l-original 230) ; was 245 ; makes it different to Flavor E
                                (begin
                                    (set! r (+ (rand 25) 230))
                                    (set! g (+ (rand 25) 230))
                                    (set! b (+ (rand 25) 230))
                                    
                                )
                                (begin
                                    (set! r (- (rand 256) 1))
                                    (set! g (- (rand 256) 1))
                                    (set! b (- (rand 256) 1))
                                )
                            )
                            
                            
                            
                        )
                    )
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
                                                (set! r (+ (rand 20) 235))
                                                (set! g (+ (rand 20) 235))
                                                (set! b (+ (rand 20) 235))
                                                
                                            )
                                            (begin
                                                (set! r (- (rand 256) 1))
                                                (set! g (- (rand 256) 1))
                                                (set! b (- (rand 256) 1))
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
                                                        (set! r (+ (rand 20) 235))
                                                        (set! g (+ (rand 20) 235))
                                                        (set! b (+ (rand 20) 235))
                                                        
                                                    )
                                                    (begin
                                                        (set! r (- (rand 256) 1))
                                                        (set! g (- (rand 256) 1))
                                                        (set! b (- (rand 256) 1))
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
                                                        (set! r (+ (rand 20) 235))
                                                        (set! g (+ (rand 20) 235))
                                                        (set! b (+ (rand 20) 235))
                                                        
                                                    )
                                                    (begin
                                                        (set! r (- (rand 256) 1))
                                                        (set! g (- (rand 256) 1))
                                                        (set! b (- (rand 256) 1))
                                                    )
                                                )
                                                
                                                
                                                
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                    
                    
                    ;(set! l-new (sqrt(+ (pow(* r R) 2) (pow (* g G) 2)  (pow (* b B) 2))))
                    (set! l-new (+ (* r R) (* g G) (* b B)))
                    (set! difference (abs (- l-new l-original)))
                )
                
                ;(set! hue (rand 360))
                ;(gimp-drawable-colorize floating hue 100 10)
                
                ;(script-fu-colorize image floating (list r g b) 100)
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
            );end of while 
            (gimp-selection-none image)
            (if (= flatten TRUE)
                (begin
                    (gimp-image-flatten image)
                )
                (begin
                )
            )
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            ;(gimp-image-undo-group-end image)                     ;undo group in one step
            (gimp-displays-flush)
            (gc); garbage cleanup
    )
) ;end of define

(script-fu-register
    "script-fu-auto-colorize-f"         ;function name
    "<Image>/Script-Fu2/Create from Image/Auto Colorize Flavor F"    ;menu register
    "Randomly colorize image with specified number of colors. \nfile:AutoColorize_FlavorF_8_02.scm"       ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2015"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image"           0                   
    SF-DRAWABLE   "Layer"           0
    SF-ADJUSTMENT "Number of colors" '(5 2 255 1 10 0 0)
    SF-TOGGLE     "Flatten newly created image" TRUE
)

; end of file