; Sparkler
; Created by Graechan and GnuTux
; 
; Contributors 
; saulgoode - Sinusoidal Pulse Generator Function
; Graechan - Density Over Actual Sparkle Count
; 
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
; Sparkler Animation Effect Beta [Update 1]
; Sparkler Animation Effect Beta [Update 2]

(define (script-fu-Sparkler-anim-effect-beta
                        image
                        drawable
                        sp-frames           ; Number Of Animation Frames
                        sp-type             ; Sparkle Sprite Type
                        sp-color-opt        ; Sparkle Coloring Option
                        sp-gradient         ; Sparkle Gradient Overlay
                        sm-sp-color         ; Small Sparkle Color
                        sm-sp-count         ; Small Sparkle Count
                        sm-sp-minval        ; Small Sparkle Min Pulse Value
                        md1-sp-color        ; Medium1 Sparkle Color
                        md1-sp-count        ; Medium1 Sparkle Count
                        md1-sp-minval       ; Medium1 Sparkle Min Pulse Value
                        rnd-md1-sp-type     ; Randomize Medium1 Sparkle Shapes
                        md2-sp-color        ; Medium2 Sparkle Color
                        md2-sp-count        ; Medium2 Sparkle Count
                        md2-sp-minval       ; Medium2 Sparkle Min Pulse Value
                        rnd-md2-sp-type     ; Randomize Medium2 Sparkle Shapes
                        lg-sp-color         ; Large Sparkle Color
                        lg-sp-count         ; Large Sparkle Count
                        lg-sp-minval        ; Large Sparkle Min Pulse Value
                        rnd-lg-sp-type      ; Randomize Large Sparkle Shapes
        )
    (gimp-image-undo-group-start image)
    ;
    ; Saul Goode's Sinusoidal Pulse Generator Function  
    ;
    ; 'start-time' should be between 0 and 2*pi (initial pixel alpha)  
    ; 'cycle-length' is the number of values produced before repeating
    ; if 'cycle-length' is 1 then the modulator returns a constant value
    ; usage:
    ; (define next-pulse-val(gen-spark-pulse-modulator 0 10 0 255))
    ; (next-pulse-val)
    ;
    (define (gen-spark-pulse-modulator start-time cycle-length minval maxval)
        (let* (  (time (min (max start-time 0) (* 2 *pi*)))
                (delta (/ (* 2 *pi*) cycle-length))
             )
            (lambda ()
                (let ((mag (+ (* (* 0.5 (- maxval minval)) (cos time)) (* 0.5 (+ minval maxval)))))
                    (set! time (if (>= (+ time delta) (* 2 *pi*))
                        (- (+ time delta) (* 2 *pi*))
                        (+ time delta)))
                    mag
                )
            )
        )
    )
    
    
    ;
    ; Create Sprite Procedure   
    ;
    (define (sp-create-sprite 
                sp-inlayer ; Input Layer
                sp-numpixels ; Number Of Pixels
                sp-xpos ; Starting X Position
                sp-ypos ; Starting Y Position
                xpos-offset ; X Position Offset Vector
                ypos-offset ; Y Position Offset Vector
                r ; Red Value
                g ; Green Value 
                b ; Blue Value
                a ; Alpha Value
       )
   
        (let* 
            (
                (loop-cnt 0)  ; Loop Counter
            )
        
            (while (< loop-cnt sp-numpixels)   ; Loop Through Each Pixel In The Sprite
            
                (sp-set-pixel 
                    sp-inlayer                                    ; Input Layer
                    (+ sp-xpos (vector-ref xpos-offset loop-cnt))  ; X Position
                    (+ sp-ypos (vector-ref ypos-offset loop-cnt))  ; Y Position
                                                           r  ; Red Value
                                                           g  ; Green Value 
                                                           b  ; Blue Value
                                                           a) ; Alpha Value 
            
                (set! loop-cnt (+ loop-cnt 1))     ; Increment Frames Count Loop Counter
            ) ; loop
        )
    ) ; return
    
    ;
    ; Define Set Pixel Procedure
    ;
    (define (sp-set-pixel sp-inlayer sp-xpos sp-ypos r g b a)
        (let* ((pix-array (cons-array 4 'byte)))
            (aset pix-array 0 r)
            (aset pix-array 1 g)
            (aset pix-array 2 b)
            (aset pix-array 3 a)
            (gimp-drawable-set-pixel sp-inlayer sp-xpos sp-ypos 4 pix-array)
        )
    ) ; return
    
    ;
    ; Declare Variables  
    ;
 (let* (
            (img -1)                                    ; Image
            (width  0)                                  ; Pattern Width 
            (height 0)                                  ; Pattern Height
            (xpos 0)                                    ; Pixel X Position
            (ypos 0)                                    ; Pixel Y Position
            (loop-cnt1 0)                               ; Loop Counter
            (loop-cnt2 0)                               ; Loop Counter
            (pulse-stime 0)                             ; Pulse Start Time
            (sp-layers (make-vector 10 0))              ; Sparkle Layers Vector
            (star-sparklex (make-vector 20 0))          ; Star Sparkle X Offset
            (star-sparkley (make-vector 20 0))          ; Star Sparkle Y Offset
            (next-pulse-val 0)                          ; Next Pulse Value
            (merge-layer -1)                            ; Merge Layer
            (sparkles 0)                                ; Store Sparkle Layers
            (frame 0)                                   ; Layer Frames
            (no-sel 0)                                  ; Check for Active Selection
            (selection-channel 0)                       ; Store any Created Selection
            (saved-selection 0)                         ; Store any Existing Selection
            (saved-sp-type sp-type)                     ; Store sp-type
            (isel 0)
            (x1 0)
            (y1 0)
            (x2 0)
            (y2 0)
            (iwidth 0)
            (iheight 0)
            (image-width 0)
            (image-height 0)
            (sample-list 0) ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
            (r 0)
            (g 0)
            (b 0)
            (alpha 0)
            (area 0)
            (iarea 0)
            (count-factor 0)
        )
        
        
        ;
        ; Sparkler Main Procedure
        ;
        
        (gimp-context-push) ; Save Context
        (gimp-context-set-gradient sp-gradient)
        
        ;(gimp-message "sp-color-opt is unknown")
        ;            (set! r (rand 255))
        ;            (set! g (rand 255))
        ;            (set! b (rand 255))
        ;            (set! alpha 255)
        ;            (set! sm-sp-color (list r g b))
        ;            (gimp-message (number->string (list-ref sm-sp-color 2)))
                    
        (if (= sp-color-opt 1)
            (begin
                (set! sm-sp-color '(255 255 255))
                (set! md1-sp-color '(255 255 255))
                (set! md2-sp-color '(255 255 255))
                (set! lg-sp-color '(255 255 255))
            )
        ) ;endif
        
        
        
        (set! no-sel (car (gimp-selection-is-empty image)))
        (cond ((= no-sel FALSE)
                (set! saved-selection (car (gimp-selection-save image))))
            (else
                (gimp-image-select-item image 2 drawable)
                (if (= (car (gimp-selection-is-empty image)) TRUE)
                    (begin
                        (gimp-image-select-rectangle image 2 
                            (car (gimp-drawable-offsets drawable)) (cadr (gimp-drawable-offsets drawable)) 
                            (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable)))
                    )
                )
                (set! selection-channel (car (gimp-selection-save image)))
            ) ;endelse
        ) ;endcond  
        
        (set! isel (car (gimp-drawable-mask-bounds drawable))) ;TRUE if there is a selection (TRUE or FALSE)
        (set! x1 (cadr (gimp-selection-bounds image)));x coordinate of upper left corner of selection bounds 
        (set! y1 (caddr (gimp-selection-bounds image)));y coordinate of upper left corner of selection bounds
        (set! x2 (cadddr (gimp-selection-bounds image)));x coordinate of lower right corner of selection bounds
        (set! y2 (cadr (cdddr  (gimp-selection-bounds image))));y coordinate of lower right corner of selection bounds  
        (set! iwidth (- x2 x1)) ;x width of the intersection
        (set! iheight (- y2 y1)) ;y height of the intersection
        
        (gimp-selection-none image)
        
        (gimp-selection-none image)
        
        ; Adjust the Sparkle Count to the Sparkle Pattern Area  
        (set! area (* 400 400))
        (set! iarea (* iwidth iheight))
        (set! count-factor (/ iarea area))
        (set! sm-sp-count (inexact->exact (round (* sm-sp-count count-factor))))
        (set! md1-sp-count (inexact->exact (round (* md1-sp-count count-factor))))
        (set! md2-sp-count (inexact->exact (round (* md2-sp-count count-factor))))
        (set! lg-sp-count (inexact->exact (round (* lg-sp-count count-factor))))
        
        
        
        (set! img (car (gimp-image-new iwidth iheight RGB))) ; New Image
        
        (set! *seed* (car (gettimeofday)))                                ; Random Number Seed From Clock
        
        ; Create Sparkle Layers & Save IDs to Vector
        ;
        (while (< loop-cnt1 sp-frames)   
            (vector-set! sp-layers loop-cnt1 (car (gimp-layer-new img iwidth iheight RGBA-IMAGE "Frame" 100 LAYER-MODE-NORMAL)))
            (gimp-image-insert-layer img (vector-ref sp-layers loop-cnt1) 0 -1)
            
            (set! loop-cnt1 (+ loop-cnt1 1))     ; Increment Frames Count Loop Counter
        ) ; loop
        
        
        ;
        ; Create Pulses In Each Frame
        ;
        
        ;
        ; Fill Each Layer With Single Pixel Sparkles
        ;
        (set! star-sparklex (vector 2 0))       ; Single Pixel X Offsets (1 Pixel) ; was (vector 0)
        (set! star-sparkley (vector 2 0))       ; Single Pixel Y Offsets (1 Pixel)
        
        (if (= sp-color-opt 2)
            (begin 
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient (+ sm-sp-count 2) FALSE)))) 
                    ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
                (set! sample-list (cddddr sample-list))
            )
        ) ;endif
        
        (gimp-progress-init "Create Small Size Sparkles" 0)
        
        (while (< loop-cnt2 sm-sp-count)
            
            (if (= sp-color-opt 2)
                (begin    
                    ;(gimp-message "sp-color-opt is 2 line 277")
                    (set! r (inexact->exact (round (* (car sample-list) 255))))
                    (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                    (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                    (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                    (set! sm-sp-color (list r g b))
                )
            ) ;endif
            
            (set! pulse-stime (rand 7))     ; Random Pulse Starting Time
            ;(gimp-message (number->string (random 2)))
            ;(gimp-message (number->string (rand 2)))
            ;(gimp-message (number->string (+ (rand (- iwidth 8)) 4)))
            ;(gimp-message (number->string (list-ref sm-sp-color 2)))
            ;(gimp-message (number->string (vector-ref star-sparklex 1)))
            ;(gimp-message (number->string (vector-ref star-sparklex 0)))
            
            (set! next-pulse-val(gen-spark-pulse-modulator pulse-stime sp-frames sm-sp-minval 255)) ; Create Pulse Function For The Pixel
            
            ;(gimp-message (number->string next-pulse-val))
            ;(gimp-message "line297")
            ;(set! next-pulse-val (round (+ (string->number next-pulse-val) 0.5)))
            ;(gimp-message (number->string (next-pulse-val)))
            
            (gimp-message (string-append (number->string loop-cnt2) "/" (number->string sm-sp-count)))
            
            (set! xpos (+ (random (- iwidth 8)) 4))   ; Random Pixel X Position (Empty 4 pixel border)
            (set! ypos (+ (random (- iheight 8)) 4))   ; Random Pixel Y Position (Empty 4 pixel border)
            
            (set! loop-cnt1 0)              ; Zero Frame Counter 
            (while (< loop-cnt1 sp-frames)  ; Activate Pixels In Each Frame
                
                (sp-create-sprite 
                    (vector-ref sp-layers loop-cnt1) ; Input Layer
                                            1 ; Nmber Of Pixels To Set  
                                            xpos ; X Position
                                            ypos ; Y Position 
                                            star-sparklex ; X Pixel Offsets Vector
                                            star-sparkley ; Y Pixel Offsets Vector
                                (list-ref sm-sp-color 0) ; Red Value
                                (list-ref sm-sp-color 1) ; Green Value 
                                (list-ref sm-sp-color 2) ; Blue Value
                                (next-pulse-val) ; Alpha 
                )
                (set! loop-cnt1 (+ loop-cnt1 1))         ; Increment Frames Counter
            ) ; loop
            
            (if (= sp-color-opt 2)
                (set! sample-list (cddddr sample-list))
            )
            (set! loop-cnt2 (+ loop-cnt2 1))     ; Increment Sparkle Loop Counter
            (gimp-progress-update (/ loop-cnt2 sm-sp-count))
        ) ; loop
        
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(gimp-message "line333")
        
        ;
        ; Fill Each Layer With Medium1 Size Random Sparkles
        ;
        (if (= rnd-md1-sp-type FALSE)
            (begin
                ;(gimp-message "line340")
                (cond
                    ((= sp-type 0)
                        (set! star-sparklex (vector 0 -1  1 -1 1 0))       ; Medium Size Sparkle/Sprite X Offsets (5 Pixels)
                        (set! star-sparkley (vector 0 -1 -1  1 1 0)))      ; Medium Size Sparkle/Sprite Y Offsets (5 Pixels)
                    ((= sp-type 1)
                        (set! star-sparklex (vector 0  0 1 -1 1))       ; Medium1 Bubble Sparkle/Sprite X Offsets (4 Pixels)
                        (set! star-sparkley (vector 1 -1 0  0 1)))      ; Medium1 Bubble Sparkle/Sprite Y Offsets (4 Pixels)
                    ((= sp-type 2)
                        (set! star-sparklex (vector 0  0 1 1  1 -1 -1 -1 2))       ; Medium1 Square Sparkle/Sprite X Offsets (8 Pixels)
                        (set! star-sparkley (vector 1 -1 0 1 -1  0  1 -1 2)))      ; Medium1 Square Sparkle/Sprite Y Offsets (8 Pixels)
                    ((= sp-type 3)
                        (set! star-sparklex (vector 0 1 1 2 2 1 0 -1 -2 -2 -2 -2 -2 -1 -1  0 0))       ; Medium1 Right-Angle-Tri Sparkle/Sprite X Offsets (16 Pixels)
                        (set! star-sparkley (vector 0 0 1 1 2 2 2  2  2  1  0 -1 -2 -2 -1 -1 0)))      ; Medium1 Right-Angle-Tri Sparkle/Sprite Y Offsets (16 Pixels)
                    ((= sp-type 4)
                        (set! star-sparklex (vector 0 -1 -1 -1  0  1  1  2  3  3 3 2 1 0))       ; Medium1 Heart Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector 1  0 -1 -2 -2 -2 -1 -2 -2 -1 0 1 2 0)))      ; Medium1 Heart Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 5)
                        (set! star-sparklex (vector 2 1 0 -1 -2 -2 -1 -2 -1))       ; Medium1 Arrow Sparkle/Sprite X Offsets (9 Pixels)
                        (set! star-sparkley (vector 0 0 0  0  0 -1 -2  1  2)))      ; Medium1 Arrow Sparkle/Sprite Y Offsets (9 Pixels)
                    ((= sp-type 6)
                        (set! star-sparklex (vector 2 1 0 -1 -2 2 1 -1 -2 -1 0 1  0))       ; Medium1 Isosceles-Tri Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector 2 2 2  2  2 1 1  1  1  0 0 0 -1)))      ; Medium1 Isosceles-Tri Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 7)
                        (set! star-sparklex (vector -1 0 1 -1 0 1  0 -1 -2  1  2))       ; Medium1 Faces Sparkle/Sprite X Offsets (11 Pixels)
                        (set! star-sparkley (vector  2 2 2  1 1 1 -1 -2 -2 -2 -2)))      ; Medium1 Faces Sparkle/Sprite Y Offsets (11 Pixels)
                ) ;endcond
            )
        ) ;endif
        
        (if (= sp-color-opt 2)
            (begin
                ;(gimp-message "line372")
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient (+ md1-sp-count 2) FALSE)))) 
                ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
                (set! sample-list (cddddr sample-list))
            )
        ) ;endif
        
        (gimp-progress-init "Create Medium1 Size Sparkles" 0)
        
     (set! loop-cnt2 0)                               ; Zero Pixels Loop Counter 
     (while (< loop-cnt2 md1-sp-count)
        
        (if (= rnd-md1-sp-type TRUE)
            (begin
                ;(gimp-message "line386 in here")
                (set! sp-type (- (rand 8) 1))
                ;(gimp-message (number->string sp-type))
                (cond
                    ((= sp-type 0)
                        (set! star-sparklex (vector 0 -1  1 -1 1 0))       ; Medium Size Sparkle/Sprite X Offsets (5 Pixels) ; added extra 0 at end
                        (set! star-sparkley (vector 0 -1 -1  1 1 0)))      ; Medium Size Sparkle/Sprite Y Offsets (5 Pixels)
                    ((= sp-type 1)
                        (set! star-sparklex (vector 0  0 1 -1 1))       ; Medium1 Bubble Sparkle/Sprite X Offsets (4 Pixels) ;1s added
                        (set! star-sparkley (vector 1 -1 0  0 1)))      ; Medium1 Bubble Sparkle/Sprite Y Offsets (4 Pixels)
                    ((= sp-type 2)
                        (set! star-sparklex (vector 0  0 1 1  1 -1 -1 -1 2))       ; Medium1 Square Sparkle/Sprite X Offsets (8 Pixels) ; 2s added
                        (set! star-sparkley (vector 1 -1 0 1 -1  0  1 -1 2)))      ; Medium1 Square Sparkle/Sprite Y Offsets (8 Pixels)
                    ((= sp-type 3)
                        (set! star-sparklex (vector 0 1 1 2 2 1 0 -1 -2 -2 -2 -2 -2 -1 -1  0 0))       ; Medium1 Right-Angle-Tri Sparkle/Sprite X Offsets (16 Pixels)
                        (set! star-sparkley (vector 0 0 1 1 2 2 2  2  2  1  0 -1 -2 -2 -1 -1 0)))      ; Medium1 Right-Angle-Tri Sparkle/Sprite Y Offsets (16 Pixels)
                    ((= sp-type 4)
                        (set! star-sparklex (vector 0 -1 -1 -1  0  1  1  2  3  3 3 2 1 1))       ; Medium1 Heart Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector 1  0 -1 -2 -2 -2 -1 -2 -2 -1 0 1 2 1)))      ; Medium1 Heart Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 5)
                        (set! star-sparklex (vector 2 1 0 -1 -2 -2 -1 -2 -1 2))       ; Medium1 Arrow Sparkle/Sprite X Offsets (9 Pixels)
                        (set! star-sparkley (vector 0 0 0  0  0 -1 -2  1  2 2)))      ; Medium1 Arrow Sparkle/Sprite Y Offsets (9 Pixels)
                    ((= sp-type 6)
                        (set! star-sparklex (vector 2 1 0 -1 -2 2 1 -1 -2 -1 0 1  0 0))       ; Medium1 Isosceles-Tri Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector 2 2 2  2  2 1 1  1  1  0 0 0 -1 0)))      ; Medium1 Isosceles-Tri Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 7)
                        (set! star-sparklex (vector -1 0 1 -1 0 1  0 -1 -2  1  2 1))       ; Medium1 Faces Sparkle/Sprite X Offsets (11 Pixels)
                        (set! star-sparkley (vector  2 2 2  1 1 1 -1 -2 -2 -2 -2 1)))      ; Medium1 Faces Sparkle/Sprite Y Offsets (11 Pixels)
                    ((= sp-type 8)
                        (gimp-message "eight line 413")
                        (quit)
                    )
                ) ;endcond
            )
        ) ;endif
        
        
        (if (= sp-color-opt 2)
            (begin
                ;(gimp-message "line425")
                (set! r (inexact->exact (round (* (car sample-list) 255))))
                (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                (set! md1-sp-color (list r g b))
            )
        ) ;endif
         
        (if (= sp-color-opt 3)
            (begin
                ;(gimp-message "line436")
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient sp-frames FALSE))))) 
                ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
        ) ;endif
        
        (set! pulse-stime (rand 7))                 ; Random Pulse Starting Time
        
        (set! next-pulse-val(gen-spark-pulse-modulator pulse-stime sp-frames md1-sp-minval 255)) ; Create Pulse Function For The Pixel
        
        (set! xpos (+ (rand (- iwidth 8)) 4))   ; Random Pixel X Position (Empty 4 pixel border)
        (set! ypos (+ (rand (- iheight 8)) 4))   ; Random Pixel Y Position (Empty 4 pixel border)
        
        ;(gimp-message "line448")
        
        (set! loop-cnt1 0)              ; Zero Frame Counter 
        (while (< loop-cnt1 sp-frames)  ; Activate Pixels In Each Frame
            
            (if (= sp-color-opt 3)
                (begin    
                    (set! r (inexact->exact (round (* (car sample-list) 255))))
                    (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                    (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                    (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                    (set! md1-sp-color (list r g b)))
            ) ;endif
            
            ;(gimp-message (number->string sp-type))
            ;(gimp-message (number->string (random 2)))
            ;(gimp-message (number->string (rand 2)))
            ;(gimp-message (number->string (+ (rand (- iwidth 8)) 4)))
            ;(gimp-message (number->string (list-ref md1-sp-color 2)))
            ;(gimp-message (number->string (vector-ref star-sparklex 1)))
            ;(gimp-message (number->string (vector-ref star-sparklex 0)))
            
            (sp-create-sprite 
                        (vector-ref sp-layers loop-cnt1) ; Input Layer
                                                         (cond
                                                         ((= sp-type 0) 5)   ; Nmber Of Pixels To Set for Default
                                                         ((= sp-type 1) 4) ; Nmber Of Pixels To Set for Bubbles
                                                         ((= sp-type 2) 8) ; Nmber Of Pixels To Set for Squares
                                                         ((= sp-type 3) 16)  ; Nmber Of Pixels To Set for Tri
                                                         ((= sp-type 4) 13)  ; Nmber Of Pixels To Set for Hearts
                                                         ((= sp-type 5) 9)   ; Nmber Of Pixels To Set for Arrows
                                                         ((= sp-type 6) 13)  ; Nmber Of Pixels To Set for Isosceles-Tri
                                                         ((= sp-type 7) 11)) ; Nmber Of Pixels To Set for Faces
                                                      xpos ; X Position
                                                      ypos ; Y Position 
                                             star-sparklex ; X Pixel Offsets Vector
                                             star-sparkley ; Y Pixel Offsets Vector
                                  (list-ref md1-sp-color 0) ; Red Value
                                  (list-ref md1-sp-color 1) ; Green Value 
                                  (list-ref md1-sp-color 2) ; Blue Value
                                                         (cond ; Alpha 
                                                         ((< sp-color-opt 3) (next-pulse-val))
                                                         ((= sp-color-opt 3) alpha)
                                                         )
           )
           
           (if (= sp-color-opt 3) (set! sample-list (cddddr sample-list)))
           
           (set! loop-cnt1 (+ loop-cnt1 1))         ; Increment Frames Counter
        ) ; loop
        
        (if (= rnd-md1-sp-type TRUE) (set! sp-type saved-sp-type))
        (if (= sp-color-opt 2) (set! sample-list (cddddr sample-list)))
        (set! loop-cnt2 (+ loop-cnt2 1))     ; Increment Sparkle Loop Counter
        (gimp-progress-update (/ loop-cnt2 md1-sp-count))
     ) ; loop
     
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(gimp-message "line507")
        ;(quit)
        
        
        ;
        ; Fill Each Layer With Medium2 Size Random Sparkles
        ;
        (if (= rnd-md2-sp-type FALSE)
            (begin
                (cond
                    ((= sp-type 0)
                        (set! star-sparklex (vector 0 -1  1 0 0))       ; Medium2 Size Sparkle/Sprite X Offsets (5 Pixels)
                        (set! star-sparkley (vector 0  0  0 1 -1)))     ; Medium2 Size Sparkle/Sprite Y Offsets (5 Pixels)
                    ((= sp-type 1)
                        (set! star-sparklex (vector 0 0 0 0 1 1 1 1 2 2 2 2 3 3 -1 -1 -1 -1 -2 -2))         ; Medium2 Bubble Sparkle/Sprite X Offsets (20 Pixels)
                        (set! star-sparkley (vector 1 2 -2 -3 1 2 -2 -3 0 1 -1 -2 0 -1 0 1 -1 -2 0 -1)))    ; Medium2 Bubble Sparkle/Sprite Y Offsets (20 Pixels)
                    ((= sp-type 2)
                        (set! star-sparklex (vector 0  0 1  1 2 2 2  2  2 -1 -1 -2 -2 -2 -2 -2))       ; Medium2 Square Sparkle/Sprite X Offsets (16 Pixels)
                        (set! star-sparkley (vector 2 -2 2 -2 0 1 2 -1 -2 -2  2  0  1  2 -1 -2)))      ; Medium2 Square Sparkle/Sprite Y Offsets (16 Pixels)
                    ((= sp-type 3)
                        (set! star-sparklex (vector 1 2 2 3 3 2 1 0 -1 -2 -2 -2 -2 -2 -2 -1 -1  0  0  1))       ; Medium2 Right-Angle-Tri 20 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 0 0 1 1 2 2 2 2  2  2  1  0 -1 -2 -3 -3 -2 -2 -1 -1)))      ; Medium2 Right-Angle-Tri 20 Sparkle/Sprite Y Offsets
                    ((= sp-type 4)
                        (set! star-sparklex (vector 2 2 1 0 -1 -1 -2 -2 -2 -1  0  0  1  1  2  3  3  3))        ; Medium2 Heart Sparkle/Sprite X Offsets (18 Pixels)
                        (set! star-sparkley (vector 0 1 2 2  1  0 -1 -2 -3 -3 -3 -2 -2 -3 -3 -3 -2 -1)))       ; Medium2 Heart Sparkle/Sprite Y Offsets (18 Pixels)
                    ((= sp-type 5)
                        (set! star-sparklex (vector -3 -2 -1 0 1 2 3  3  2  1 3 2 1))       ; Medium2 Arrow Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector  0  0  0 0 0 0 0 -1 -2 -3 1 2 3)))      ; Medium2 Arrow Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 6)
                        (set! star-sparklex (vector 3 2 1 0 -1 -2 -3 -3 -2 2 3 -2 -1 1 2 -1 0 1  0))       ; Medium2 Isosceles-Tri Sparkle/Sprite X Offsets (19 Pixels)
                        (set! star-sparkley (vector 3 3 3 3  3  3  3  2  2 2 2  1  1 1 1  0 0 0 -1)))      ; Medium2 Isosceles-Tri Sparkle/Sprite Y Offsets (19 Pixels)
                    ((= sp-type 7)
                        (set! star-sparklex (vector -3 -2 -1 0 1 2 3 -1 0 1  0  0 -3 -2 -2 -3  2  3  3  2))       ; Medium2 Faces Sparkle/Sprite X Offsets (20 Pixels)
                        (set! star-sparkley (vector  3  2  2 2 2 2 3  1 1 1 -1 -2 -3 -3 -2 -2 -3 -3 -2 -2)))      ; Medium2 Faces Sparkle/Sprite Y Offsets (20 Pixels)
                ) ;endcond
            )
        ) ;endif
        
        (if (= sp-color-opt 2)
            (begin 
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient (+ md2-sp-count 2) FALSE)))) 
                ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
                (set! sample-list (cddddr sample-list))
            )
        ) ;endif
        
        ;(gimp-message "line553")
        ;(gimp-displays-flush)
        
        (gimp-progress-init "Create Medium2 Size Sparkles" 0)
        
     (set! loop-cnt2 0)                               ; Zero Pixels Loop Counter 
     (while (< loop-cnt2 md2-sp-count)
        
        (if (= rnd-md2-sp-type TRUE)
            (begin
                (set! sp-type (- (rand 8) 1))
                (cond
                    ((= sp-type 0)
                        (set! star-sparklex (vector 0 -1  1 0 0))       ; Medium2 Size Sparkle/Sprite X Offsets (5 Pixels)
                        (set! star-sparkley (vector 0  0  0 1 -1)))     ; Medium2 Size Sparkle/Sprite Y Offsets (5 Pixels)
                    ((= sp-type 1)
                        (set! star-sparklex (vector 0 0 0 0 1 1 1 1 2 2 2 2 3 3 -1 -1 -1 -1 -2 -2))         ; Medium2 Bubble Sparkle/Sprite X Offsets (20 Pixels)
                        (set! star-sparkley (vector 1 2 -2 -3 1 2 -2 -3 0 1 -1 -2 0 -1 0 1 -1 -2 0 -1)))    ; Medium2 Bubble Sparkle/Sprite Y Offsets (20 Pixels)
                    ((= sp-type 2)
                        (set! star-sparklex (vector 0  0 1  1 2 2 2  2  2 -1 -1 -2 -2 -2 -2 -2))       ; Medium2 Square Sparkle/Sprite X Offsets (16 Pixels)
                        (set! star-sparkley (vector 2 -2 2 -2 0 1 2 -1 -2 -2  2  0  1  2 -1 -2)))      ; Medium2 Square Sparkle/Sprite Y Offsets (16 Pixels)
                    ((= sp-type 3)
                        (set! star-sparklex (vector 1 2 2 3 3 2 1 0 -1 -2 -2 -2 -2 -2 -2 -1 -1  0  0  1))       ; Medium2 Right-Angle-Tri 20 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 0 0 1 1 2 2 2 2  2  2  1  0 -1 -2 -3 -3 -2 -2 -1 -1)))      ; Medium2 Right-Angle-Tri 20 Sparkle/Sprite Y Offsets
                    ((= sp-type 4)
                        (set! star-sparklex (vector 2 2 1 0 -1 -1 -2 -2 -2 -1  0  0  1  1  2  3  3  3))        ; Medium2 Heart Sparkle/Sprite X Offsets (18 Pixels)
                        (set! star-sparkley (vector 0 1 2 2  1  0 -1 -2 -3 -3 -3 -2 -2 -3 -3 -3 -2 -1)))       ; Medium2 Heart Sparkle/Sprite Y Offsets (18 Pixels)
                    ((= sp-type 5)
                        (set! star-sparklex (vector -3 -2 -1 0 1 2 3  3  2  1 3 2 1))       ; Medium2 Arrow Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector  0  0  0 0 0 0 0 -1 -2 -3 1 2 3)))      ; Medium2 Arrow Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 6)
                        (set! star-sparklex (vector 3 2 1 0 -1 -2 -3 -3 -2 2 3 -2 -1 1 2 -1 0 1  0))       ; Medium2 Isosceles-Tri Sparkle/Sprite X Offsets (19 Pixels)
                        (set! star-sparkley (vector 3 3 3 3  3  3  3  2  2 2 2  1  1 1 1  0 0 0 -1)))      ; Medium2 Isosceles-Tri Sparkle/Sprite Y Offsets (19 Pixels)
                    ((= sp-type 7)
                        (set! star-sparklex (vector -3 -2 -1 0 1 2 3 -1 0 1  0  0 -3 -2 -2 -3  2  3  3  2))       ; Medium2 Faces Sparkle/Sprite X Offsets (20 Pixels)
                        (set! star-sparkley (vector  3  2  2 2 2 2 3  1 1 1 -1 -2 -3 -3 -2 -2 -3 -3 -2 -2)))      ; Medium2 Faces Sparkle/Sprite Y Offsets (20 Pixels)
                    ((= sp-type 8)
                        (gimp-message "eight line590")
                        (quit))
                    ((= sp-type -1)
                        (gimp-message "minus1 line593")
                        (quit))
                    
                ) ;endcond
            )
         ) ;endif
         ;(gimp-message "line599")
         
         (if (= sp-color-opt 2)
            (begin    
                (set! r (inexact->exact (round (* (car sample-list) 255))))
                (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                (set! md2-sp-color (list r g b))
            )
         ) ;endif
         
         (if (= sp-color-opt 3)
            (begin
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient sp-frames FALSE))))
            ) 
                ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
         ) ;endif
         
         (set! pulse-stime (rand 7))                 ; Random Pulse Starting Time
         
         (set! next-pulse-val(gen-spark-pulse-modulator pulse-stime sp-frames md2-sp-minval 255)) ; Create Pulse Function For The Pixel
         
         (set! xpos (+ (rand (- iwidth 8)) 4))   ; Random Pixel X Position (Empty 4 pixel border)
         (set! ypos (+ (rand (- iheight 8)) 4))   ; Random Pixel Y Position (Empty 4 pixel border)
         
         (set! loop-cnt1 0)              ; Zero Frame Counter 
         (while (< loop-cnt1 sp-frames)  ; Activate Pixels In Each Frame
            
            (if (= sp-color-opt 3)
                (begin    
                    (set! r (inexact->exact (round (* (car sample-list) 255))))
                    (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                    (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                    (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                    (set! md2-sp-color (list r g b))
                )
            ) ;endif
            
            (sp-create-sprite 
                        (vector-ref sp-layers loop-cnt1) ; Input Layer
                                                         (cond
                                                         ((= sp-type 0) 5)   ; Nmber Of Pixels To Set for Default
                                                         ((= sp-type 1) 20)  ; Nmber Of Pixels To Set for Bubbles
                                                         ((= sp-type 2) 16)  ; Nmber Of Pixels To Set for Squares
                                                         ((= sp-type 3) 20)  ; Nmber Of Pixels To Set for Tri
                                                         ((= sp-type 4) 18)  ; Nmber Of Pixels To Set for Hearts
                                                         ((= sp-type 5) 13)  ; Nmber Of Pixels To Set for Arrows
                                                         ((= sp-type 6) 19)  ; Nmber Of Pixels To Set for Isosceles-Tri
                                                         ((= sp-type 7) 20)) ; Nmber Of Pixels To Set for Faces
                                                      xpos ; X Position
                                                      ypos ; Y Position 
                                             star-sparklex ; X Pixel Offsets Vector
                                             star-sparkley ; Y Pixel Offsets Vector
                                  (list-ref md2-sp-color 0) ; Red Value
                                  (list-ref md2-sp-color 1) ; Green Value 
                                  (list-ref md2-sp-color 2) ; Blue Value
                                                         (cond ; Alpha 
                                                         ((< sp-color-opt 3) (next-pulse-val))
                                                         ((= sp-color-opt 3) alpha)
                                                         )
            )
            
            (if (= sp-color-opt 3) (set! sample-list (cddddr sample-list)))
            
            (set! loop-cnt1 (+ loop-cnt1 1))         ; Increment Frames Counter
         ) ; loop
         
         (if (= rnd-md2-sp-type TRUE) (set! sp-type saved-sp-type))
         (if (= sp-color-opt 2) (set! sample-list (cddddr sample-list)))
         (set! loop-cnt2 (+ loop-cnt2 1))     ; Increment Sparkle Loop Counter
         (gimp-progress-update (/ loop-cnt2 md2-sp-count))
     ) ; loop
     
     ;(gimp-message "line673")
     ;(gimp-displays-flush)
     ;(gimp-display-new img)
     ;(quit)
     ;
     ; Fill Each Layer With Large Size Random Sparkles
     ;
     (if (= rnd-lg-sp-type FALSE)
        (begin
            (cond
                ((= sp-type 0)
                    (set! star-sparklex (vector 0 -1 1 0 -1 1  0 -1  1 -2 2  0 0))    ; Large Sparkle/Sprite X Offsets (13 Pixels)
                    (set! star-sparkley (vector 0  0 0 1  1 1 -1 -1 -1  0 0 -2 2)))   ; Large Sparkle/Sprite Y Offsets (13 Pixels)
                ((= sp-type 1)
                    (set! star-sparklex (vector 0 0  0  0 1 1  1  1 2 2  2  2 3 3  3  3 4  4 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3))  ; Bubble Sparkle/28 Sprite X Offsets 
                    (set! star-sparkley (vector 2 3 -3 -4 2 3 -3 -4 1 2 -2 -3 0 1 -1 -2 0 -1  1  2 -2 -3  0  1 -1 -2  0 -1))) ; Bubble Sparkle/28 Sprite Y Offsets
                ((= sp-type 2)
                    (set! star-sparklex (vector 0  0 1  1 2  2 3 3 3 3  3  3  3 -1 -1 -2 -2 -3 -3 -3 -3 -3 -3 -3))     ; Large Square Sparkle/24 Sprite X Offsets 
                    (set! star-sparkley (vector 3 -3 3 -3 3 -3 0 1 2 3 -1 -2 -3  3 -3  3 -3  0  1  2  3 -1 -2 -3)))    ; Large Square Sparkle/24 Sprite Y Offsets
                ((= sp-type 3)
                    (set! star-sparklex (vector 2 3 3 4 4 4 3 2 1 0 -1 -2 -3 -3 -3 -3 -3 -3 -3 -3 -2 -1 -1  0  0  1  1  2))  ; Large Right-Angle-Tri 28 Sparkle/Sprite X Offsets
                    (set! star-sparkley (vector 0 0 1 1 2 3 3 3 3 3  3  3  3  2  1  0 -1 -2 -3 -4 -4 -4 -3 -3 -2 -2 -1 -1))) ; Large Right-Angle-Tri 28 Sparkle/Sprite Y Offsets
                ((= sp-type 4)
                    (set! star-sparklex (vector 3 2 2 1 0 -1 -1 -2 -3 -3 -3 -3 -2 -1  0  0  0  1  1  1  2  3  4  4  4  4))  ; Large Heart 26 Sparkle/Sprite X Offsets
                    (set! star-sparkley (vector 0 1 2 3 3  2  1  0 -1 -2 -3 -4 -4 -4 -4 -3 -2 -2 -3 -4 -4 -4 -4 -3 -2 -1))) ; Large Heart 26 Sparkle/Sprite Y Offsets
                ((= sp-type 5)
                    (set! star-sparklex (vector 0 0 0 0 0  0  0  0  0 -1 -2 -3 -4  4  3  2  1))  ; Large Arrow 17 Sparkle/Sprite X Offsets
                    (set! star-sparkley (vector 4 3 2 1 0 -1 -2 -3 -4 -4 -3 -2 -1 -1 -2 -3 -4))) ; Large Arrow 17 Sparkle/Sprite Y Offsets
                ((= sp-type 6)
                    (set! star-sparklex (vector 4 3 2 1 0 -1 -2 -3 -4 -4 -3 3 4 -3 -2 2 3 1 2 -1 -2 -1 0 1  0))  ; Large Isosceles-tri 25 Sparkle/Sprite X Offsets
                    (set! star-sparkley (vector 4 4 4 4 4  4  4  4  4  3  3 3 3  2  2 2 2 1 1  1  1  0 0 0 -1))) ; Large Isosceles-tri 25 Sparkle/Sprite Y Offsets
                ((= sp-type 7)
                    (set! star-sparklex (vector -3 -2 -1 0 1 2 3 -1 0 1 0 0  0  0 -4 -3 -2 -2 -2 -3 -4 -4  2  3  4  4  4  3  2  2))  ; Large Faces 30 Sparkle/Sprite X
                    (set! star-sparkley (vector  1  2  2 2 2 2 1  3 3 3 4 0 -1 -2 -4 -4 -4 -3 -2 -2 -2 -3 -4 -4 -4 -3 -2 -2 -2 -3))) ; Large Faces 30 Sparkle/Sprite Y
            ) ;endcond
        )
     ) ;endif
     
     (if (= sp-color-opt 2)
        (begin 
            (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient (+ lg-sp-count 2) FALSE)))) 
            ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
            (set! sample-list (cddddr sample-list))
        )
     ) ;endif
     
     (gimp-progress-init "Create Large Sparkles" 0)
     
     (set! loop-cnt2 0)                               ; Zero Pixels Loop Counter 
     
     (while (< loop-cnt2 lg-sp-count)
        
        (if (= rnd-lg-sp-type TRUE)
            (begin
                (set! sp-type (- (random 8) 1))
                (cond
                    ((= sp-type 0)
                        (set! star-sparklex (vector 0 -1 1 0 -1 1  0 -1  1 -2 2  0 0))    ; Large Sparkle/Sprite X Offsets (13 Pixels)
                        (set! star-sparkley (vector 0  0 0 1  1 1 -1 -1 -1  0 0 -2 2)))   ; Large Sparkle/Sprite Y Offsets (13 Pixels)
                    ((= sp-type 1)
                        (set! star-sparklex (vector 0 0  0  0 1 1  1  1 2 2  2  2 3 3  3  3 4  4 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3))  ; Bubble Sparkle/28 Sprite X Offsets 
                        (set! star-sparkley (vector 2 3 -3 -4 2 3 -3 -4 1 2 -2 -3 0 1 -1 -2 0 -1  1  2 -2 -3  0  1 -1 -2  0 -1))) ; Bubble Sparkle/28 Sprite Y Offsets
                    ((= sp-type 2)
                        (set! star-sparklex (vector 0  0 1  1 2  2 3 3 3 3  3  3  3 -1 -1 -2 -2 -3 -3 -3 -3 -3 -3 -3))     ; Large Square Sparkle/24 Sprite X Offsets 
                        (set! star-sparkley (vector 3 -3 3 -3 3 -3 0 1 2 3 -1 -2 -3  3 -3  3 -3  0  1  2  3 -1 -2 -3)))    ; Large Square Sparkle/24 Sprite Y Offsets
                    ((= sp-type 3)
                        (set! star-sparklex (vector 2 3 3 4 4 4 3 2 1 0 -1 -2 -3 -3 -3 -3 -3 -3 -3 -3 -2 -1 -1  0  0  1  1  2))  ; Large Right-Angle-Tri 28 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 0 0 1 1 2 3 3 3 3 3  3  3  3  2  1  0 -1 -2 -3 -4 -4 -4 -3 -3 -2 -2 -1 -1))) ; Large Right-Angle-Tri 28 Sparkle/Sprite Y Offsets
                    ((= sp-type 4)
                        (set! star-sparklex (vector 3 2 2 1 0 -1 -1 -2 -3 -3 -3 -3 -2 -1  0  0  0  1  1  1  2  3  4  4  4  4))  ; Large Heart 26 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 0 1 2 3 3  2  1  0 -1 -2 -3 -4 -4 -4 -4 -3 -2 -2 -3 -4 -4 -4 -4 -3 -2 -1))) ; Large Heart 26 Sparkle/Sprite Y Offsets
                    ((= sp-type 5)
                        (set! star-sparklex (vector 0 0 0 0 0  0  0  0  0 -1 -2 -3 -4  4  3  2  1))  ; Large Arrow 17 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 4 3 2 1 0 -1 -2 -3 -4 -4 -3 -2 -1 -1 -2 -3 -4))) ; Large Arrow 17 Sparkle/Sprite Y Offsets
                    ((= sp-type 6)
                        (set! star-sparklex (vector 4 3 2 1 0 -1 -2 -3 -4 -4 -3 3 4 -3 -2 2 3 1 2 -1 -2 -1 0 1  0))  ; Large Isosceles-tri 25 Sparkle/Sprite X Offsets
                        (set! star-sparkley (vector 4 4 4 4 4  4  4  4  4  3  3 3 3  2  2 2 2 1 1  1  1  0 0 0 -1))) ; Large Isosceles-tri 25 Sparkle/Sprite Y Offsets
                    ((= sp-type 7)
                        (set! star-sparklex (vector -3 -2 -1 0 1 2 3 -1 0 1 0 0  0  0 -4 -3 -2 -2 -2 -3 -4 -4  2  3  4  4  4  3  2  2))  ; Large Faces 30 Sparkle/Sprite X
                        (set! star-sparkley (vector  1  2  2 2 2 2 1  3 3 3 4 0 -1 -2 -4 -4 -4 -3 -2 -2 -2 -3 -4 -4 -4 -3 -2 -2 -2 -3))) ; Large Faces 30 Sparkle/Sprite Y
                ) ;endcond
            )
        ) ;endif
        
        (if (= sp-color-opt 2)
            (begin    
                (set! r (inexact->exact (round (* (car sample-list) 255))))
                (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                (set! lg-sp-color (list r g b)))
        ) ;endif
        
        (if (= sp-color-opt 3)
            (begin
                (set! sample-list (vector->list (cadr (gimp-gradient-get-uniform-samples sp-gradient sp-frames FALSE))))) 
                ;=Color samples: { R1, G1, B1, A1, ..., Rn, Gn, Bn, An }
        ) ;endif
        
        (set! pulse-stime (rand 7))                 ; Random Pulse Starting Time
        
        (set! next-pulse-val(gen-spark-pulse-modulator pulse-stime sp-frames lg-sp-minval 255)) ; Create Pulse Function For The Pixel
        
        (set! xpos (+ (random (- iwidth 13)) 6))   ; Random Pixel X Position (Empty 4 pixel border) ; was 8 & 4
        (set! ypos (+ (random (- iheight 13)) 6))   ; Random Pixel Y Position (Empty 4 pixel border)
        
        (set! loop-cnt1 0)              ; Zero Frame Counter 
        (while (< loop-cnt1 sp-frames)  ; Activate Pixels In Each Frame
            
            (if (= sp-color-opt 3)
                (begin    
                    (set! r (inexact->exact (round (* (car sample-list) 255))))
                    (set! g (inexact->exact (round (* (cadr sample-list) 255))))
                    (set! b (inexact->exact (round (* (caddr sample-list) 255))))
                    (set! alpha (inexact->exact (round (* (cadddr sample-list) 255))))
                    (set! lg-sp-color (list r g b)))
            ) ;endif
            
            (sp-create-sprite 
                          (vector-ref sp-layers loop-cnt1) ; Input Layer
                                                         (cond
                                                            ((= sp-type 0) 13)   ; Nmber Of Pixels To Set for Default
                                                            ((= sp-type 1) 28)   ; Nmber Of Pixels To Set for Bubbles
                                                            ((= sp-type 2) 24)   ; Nmber Of Pixels To Set for Squares
                                                            ((= sp-type 3) 28)   ; Nmber Of Pixels To Set for Tri
                                                            ((= sp-type 4) 26)   ; Nmber Of Pixels To Set for Hearts
                                                            ((= sp-type 5) 17)   ; Nmber Of Pixels To Set for Arrows
                                                            ((= sp-type 6) 25)   ; Nmber Of Pixels To Set for Isosceles-Tri
                                                            ((= sp-type 7) 30))  ; Nmber Of Pixels To Set for Faces
                                                      xpos ; X Position
                                                      ypos ; Y Position 
                                             star-sparklex ; X Pixel Offsets Vector
                                             star-sparkley ; Y Pixel Offsets Vector
                                  (list-ref lg-sp-color 0) ; Red Value
                                  (list-ref lg-sp-color 1) ; Green Value 
                                  (list-ref lg-sp-color 2) ; Blue Value                                                     
                                                        (cond ; Alpha 
                                                            ((< sp-color-opt 3) (next-pulse-val))
                                                            ((= sp-color-opt 3) alpha)) 
            )
            
            (if (= sp-color-opt 3)
                (begin
                    ;(gimp-message "line816")
                    (set! sample-list (cddddr sample-list))
                )
            )
            
            (set! loop-cnt1 (+ loop-cnt1 1))         ; Increment Frames Counter
         ) ; loop
         
         (if (= rnd-lg-sp-type TRUE) (set! sp-type saved-sp-type))
         (if (= sp-color-opt 2) (set! sample-list (cddddr sample-list)))
         (set! loop-cnt2 (+ loop-cnt2 1))     ; Increment Sparkle Loop Counter
         
         (gimp-progress-update (/ loop-cnt2 lg-sp-count))
         
     ) ; loop
     ;(gimp-message "line831") 
     
     (if (= sp-color-opt 1)
        (begin
            (gimp-context-set-sample-criterion SELECT-CRITERION-A)
            (gimp-context-set-sample-threshold 0.3)
            (gimp-image-select-color img CHANNEL-OP-REPLACE (vector-ref sp-layers 0) '(252 252 252))
            
            ;(gimp-selection-invert img)
            (map (lambda (x) (gimp-edit-blend x BLEND-CUSTOM LAYER-MODE-OVERLAY GRADIENT-LINEAR 100 0 REPEAT-SAWTOOTH FALSE FALSE 3 0.2 TRUE 0 0 0 iheight)) 
             (vector->list (cadr (gimp-image-get-layers img))))
            (gimp-selection-none img)
        )
     ) ;edif
     (gimp-progress-end)
     
     ;(gimp-message "line848")
;	(script-fu-reverse-layers img (car (gimp-image-get-active-layer img)))    ; Reverse Sparkle Layer Stack
     (set! sparkles (vector->list (cadr (gimp-image-get-layers img))))         ;Store Sparkle Layers
     (set! sparkles (reverse sparkles))
     
     ;------------------------------------------------------------------------------ Create the Animation
    
    ;(gimp-message "line855")
    (set! loop-cnt1 0)              ; Zero Frame Counter 
    (while (< loop-cnt1 sp-frames)  ; Activate Pixels In Each Frame
        (set! frame (car (gimp-layer-copy drawable TRUE)))
        (gimp-image-insert-layer image frame 0 -1)
        (gimp-item-set-name frame "Frame")
        (gimp-edit-copy (car sparkles))
        (gimp-context-set-pattern (caadr (gimp-patterns-get-list "")))
        (cond ((= no-sel FALSE) 
                (gimp-image-select-item image 2 saved-selection))                            ; Restore Selection
            (else (gimp-image-select-item image 2 selection-channel))                        ; Create Selection
        ) ;endcond
        
        (gimp-edit-bucket-fill frame BUCKET-FILL-PATTERN LAYER-MODE-NORMAL 100 0 0 0 0) ; Fill with pattern
        (gimp-selection-none image)
        (set! sparkles (cdr sparkles))
        (set! loop-cnt1 (+ loop-cnt1 1))         ; Increment Frames Counter
    ) ; loop
    
    ;(gimp-message "line869")
    (gimp-image-remove-layer image drawable)                            ; Remove the Drawable
    (if (= no-sel FALSE)
        (begin
            (gimp-image-select-item image 2 saved-selection)                             ; Restore Selection
            (gimp-image-remove-channel image saved-selection))                 ; Remove Saved-Selection channel
        (begin
            (gimp-image-remove-channel image selection-channel))                 ; Remove Created Selection Channel
    ) ;endif
    
    (gimp-image-delete img) ; Delete the Sparkle Image
    
    (gimp-displays-flush)  ; Flush Display
    (gimp-image-undo-group-end image) 
    (gimp-context-pop)     ; Restore Context  
    
 ); end let
); end define
;
;
; Register Sparkler Script
;
(script-fu-register "script-fu-Sparkler-anim-effect-beta"            
            "Sparkler Animation Effect..."
            "Sparkle Effect. Sparkle counts are per 400x400 block adjusted for the size of the image. \nfile:Sparkler-Animation-Effect_02.scm"
            "Graechan GnuTux"
            "GPL"
            "2015"
            "RGB*"
            SF-IMAGE        "image"      0
            SF-DRAWABLE     "drawable"   0
            SF-ADJUSTMENT   "Frames"                    '(6 3 10 1 1 0 0)
            SF-OPTION       "Sparkle Shapes Set"        '("Default (Stars)" "Bubbles" "Squares" "Triagular" "Hearts" "Arrows" "Isosceles-Triangle" "Faces")
            SF-OPTION       "Sparkle Coloring"          '("From Color selectors" "Using Sparkle Gradient" "Party Mix From Gradient" "Chameleon Colors")
            SF-GRADIENT     "Sparkle Gradient"          "Full saturation spectrum CCW"
            SF-COLOR        "Small Sparkle Color"       '(255 255 255)
            SF-ADJUSTMENT   "Small Sparkle Count"       '(100 0 400 1 10 0 0)
            SF-ADJUSTMENT   "Small Sparkle Min Pulse Value"     '(0 0 255 1 10 0 0)
            SF-COLOR        "Medium1 Sparkle Color"     '(255 255 255)
            SF-ADJUSTMENT   "Medium1 Sparkle Count"     '(40 0 400 1 10 0 0)
            SF-ADJUSTMENT   "Medium1 Sparkle Min Pulse Value"   '(0 0 255 1 10 0 0)
            SF-TOGGLE       "Randomize Medium1 Sparkle Shapes"  FALSE
            SF-COLOR        "Medium2 Sparkle Color"     '(255 255 255)
            SF-ADJUSTMENT   "Medium2 Sparkle Count"     '(40 0 400 1 10 0 0)
            SF-ADJUSTMENT   "Medium2 Sparkle Min Pulse Value"   '(3 0 255 1 10 0 0)
            SF-TOGGLE       "Randomize Medium2 Sparkle Shapes"  FALSE
            SF-COLOR        "Large Sparkle Color"       '(255 255 255)
            SF-ADJUSTMENT   "Large Sparkle Count"       '(40 0 400 1 10 0 0)
            SF-ADJUSTMENT   "Large Sparkle Min Pulse Value"     '(0 0 255 1 10 0 0)
            SF-TOGGLE       "Randomize Large Sparkle Shapes"    FALSE
) ;End register

(script-fu-menu-register "script-fu-Sparkler-anim-effect-beta" "<Image>/Script-Fu2/Animation")

;;