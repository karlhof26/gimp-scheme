;;
;; Stripes, V1.0
;
;; Karl Hofmeyr (karl@hofmeyr.net)
;; (C) 2016, Cape Town, South Africa
;
;; This script was tested with Gimp 2.8
;;
;; New versions will be distributed from <http:// registry.gimp.org/> only
;;
;;This program is free software; you can redistribute it and/or modify
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

(define (fu-draw-stripe 
        inImage
        inLayer
        blockx
        blocky
        blockwidth
        blockheight
        blockvert
        stripeColor
        stripepos)
    (let* (
            ; variables go here
                (thisstripex 0)
                (thisstripey 0)
            )
        ; calcuate the position of this stripe
        (if (= blockvert FALSE)
            (begin
                (set! thisstripex blockx)
                (set! thisstripey (+ blocky (* (- stripepos 1) blockheight)))
            )
            (begin
                (set! thisstripex (+ blockx (* stripepos blockwidth)))
                (set! thisstripey blocky)
            )
        )
        
        (gimp-message "GG - draw a stripe 1")
        (gimp-image-select-rectangle inImage CHANNEL-OP-REPLACE thisstripex thisstripey blockwidth blockheight)
        (gimp-message "C set the color")
        (gimp-context-set-foreground stripeColor)
        (gimp-message "D Fill the color")
        (gimp-edit-fill inLayer FILL-FOREGROUND)
    )
)

(define (fu-draw-first-set 
            firstImage
            firstLayer
            mainrepeat
            firstx
            firsty
            firstwidth
            firstheight
            firstvert
            numcolors
            colorA
            colorB
            colorC
            colorD
            colorE
            colorF)
    (let*(
            (xstart 0)
            (xstep 0)
            (stripe1x 1)
            (stripe1y 1)
            (stripe2x 0)
            (stripe2y 0)
            (numrowsinset 2)
            (newstripeheight 0)
            (newstripestart 0)
            (count 1)
            (stripewidth 1)
            (stripeheight 1)
            (firstHeight (car (gimp-image-height firstImage)))
            (basestart 0)
            (finalstart 1)
            (totalslots 0)
            )
            
            (if (= firstvert FALSE)
                (begin
                    (set! stripewidth firstwidth)
                    (set! stripeheight firstheight)
                    (set! newstripestart firsty)
                    (set! totalslots (- (* numcolors 2) 2))
                )
            )
        
        (set! count 1)
        (set! basestart firsty)
        (while (<= count mainrepeat)
            (begin
                               
                (set! newstripestart 1)
                (set! newstripestart (* stripeheight 0))
                (set! finalstart (+ basestart newstripestart))
                (gimp-message "final start is")
                (gimp-message (number->string finalstart))    
                (gimp-message "B - draw row 1")
                (gimp-image-select-rectangle firstImage CHANNEL-OP-REPLACE firstx finalstart stripewidth stripeheight)
                (gimp-message "C set the color")
                (gimp-context-set-foreground colorA)
                (gimp-message "D Fill the color")
                (gimp-edit-fill firstLayer FILL-FOREGROUND)
                (gimp-selection-none firstImage)
                
                (if (= firstvert FALSE)
                    (begin 
                        (gimp-message "inside first vert")
                        (gimp-message (number->string count))
                        (set! newstripestart (* stripeheight 1) )
                        (gimp-message "debug1")
                        (set! finalstart (+ basestart newstripestart))
                        (gimp-message "number here isit ")
                        (gimp-message (number->string count))
                        (gimp-message (number->string newstripestart))
                        (gimp-message (number->string basestart))
                        (gimp-message (number->string finalstart))
                    )
                )
                (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                ;;(gimp-message "Set colorB")
                (gimp-context-set-foreground colorB)
                (gimp-edit-fill firstLayer FILL-FOREGROUND)
                
                (if (= firstvert FALSE)
                    (begin
                        (gimp-message "inside second vert")
                        (set! newstripestart  (* stripeheight 2))
                        (set! finalstart (+ basestart newstripestart))
                        (gimp-message "number here")
                        (gimp-message (number->string finalstart))
                    )
                )
                (gimp-selection-none firstImage)
                
                (if (> numcolors 2)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorC)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)               
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside third vert")
                                (set! newstripestart (* stripeheight 3) )
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                                (gimp-message (number->string totalslots))
                            )
                        )
                    )
                )
                (gimp-selection-none firstImage)
                
                (if (> numcolors 3)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorD)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside fourth vert")
                                (set! newstripestart (* stripeheight 4))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                                (gimp-message (number->string totalslots))
                            )
                        )
                    )
                )
                
                (gimp-selection-none firstImage)
                
                (if (> numcolors 4)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorE)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside fifth vert")
                                (set! newstripestart (* stripeheight 5))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                            )
                        )
                    )
                    (begin
                        (gimp-message "skipped five")
                    )
                )
                
                (gimp-selection-none firstImage)
                (if (> numcolors 5)
                    (begin
                        (gimp-message "inside sixth part 1")
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorF)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside sixth vert")
                                (set! newstripestart (* stripeheight 6))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                            )
                        )
                    )
                    (begin
                        (gimp-message "skipped six")
                    )
                )
                
                (gimp-selection-none firstImage)
                (if (> numcolors 5)
                    (begin
                        (gimp-message "inside sixth part 2")
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorE)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside seventh vert")
                                ;;(set! newstripestart (* stripeheight (- totalslots 3)))
                                (set! newstripestart (+ newstripestart stripeheight))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                                (gimp-message (number->string totalslots))
                            )
                        )
                    )
                )
                (gimp-selection-none firstImage)
                (if (> numcolors 4)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorD)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside eigthth vert")
                                ;;(set! newstripestart (* stripeheight (- totalslots 2)))
                                (set! newstripestart (+ newstripestart stripeheight))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                                (gimp-message (number->string totalslots))
                            )
                        )
                    )
                )
                (gimp-selection-none firstImage)
                (if (> numcolors 3)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorC)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (if (= firstvert FALSE)
                            (begin
                                (gimp-message "inside ninth vert")
                               ;; (set! newstripestart (* stripeheight (- totalslots 1)))
                                (set! newstripestart (+ newstripestart stripeheight))
                                (set! finalstart (+ basestart newstripestart))
                                (gimp-message (number->string finalstart))
                            )
                        )
                    )
                )
                
                (gimp-selection-none firstImage)
                (if (> numcolors 2)
                    (begin
                        (gimp-image-select-rectangle firstImage CHANNEL-OP-ADD firstx finalstart stripewidth stripeheight)
                        (gimp-context-set-foreground colorB)
                        (gimp-edit-fill firstLayer FILL-FOREGROUND)
                        (gimp-message "inside tenth vert")
                        (set! newstripestart (* stripeheight (- totalslots 0)))
                        (set! finalstart (+ basestart newstripestart))
                        (gimp-message (number->string finalstart))
                    )
                )
                
                (set! count (+ 1 count))
                (if (= firstvert FALSE)
                    (begin
                        (set! newstripestart (* stripeheight (- (* numcolors 2) 2)))
                        (set! finalstart (+ basestart newstripestart))
                        (set! basestart (+ basestart newstripestart))
                        (gimp-message "final numbers:")
                        (gimp-message (number->string finalstart))
                        (gimp-message (number->string basestart))
                    )
                )
                ; check that image has not been exceeded
                ; if newbasestart is higher than height of image
                (if (> basestart firstHeight)
                    (begin
                        (gimp-message "height exceeded. stop loops")
                        (set! count (+ mainrepeat 1))
                    )
                )
                
                
                (gimp-displays-flush)
            )
        )
        (gimp-displays-flush)
        (gimp-display-new firstImage)
       ;; (quit)
    )
)

(define (determinecolor valx colorA
        colorB
        colorC
        colorD
        colorE
        colorF)
    (let* (
                (finalcolor '(255 255 255))
            )
        (set! finalcolor
           (cond 
                (( equal? valx 1 ) colorA)
                (( equal? valx 2 ) colorB)
                (( equal? valx 3 ) colorC)
                (( equal? valx 4 ) colorD)
                (( equal? valx 5 ) colorE)
                (( equal? valx 6 ) colorF)
            )
        ); end set
;        (set! finalcolor '(100 100 100))
        ; return the finalcolor
        finalcolor        
    )
)

(define (script-fu-stripes inImage inLayer 
        stripeno
        stripetype
        numcolors
        rowheight
        colorA
        colorB
        colorC
        colorD
        colorE
        colorF)
        ;;
        
  (let* (
            (TheImage (car (gimp-image-duplicate inImage)))
            (TheLayer (car (gimp-image-flatten TheImage)))
            
            (TheWidth (car (gimp-image-width TheImage)))
            (TheHeight (car (gimp-image-height TheImage)))
            ;
            ;(isrgb (car (gimp-drawable-is-rgb TheLayer)))
            ; W-Border = % of widht, H-Border = % of height (asymmetric)
            ;
            ;(outer-border-width (/ (* TheWidth inOuterPercentWidth) 100))
            ;(outer-border-top-height (/ (* TheHeight inOuterPercentWidth) 100))
            ;
            ; W-Border and H-Border = % of widht (symmetric)
            ;
            ;(outer-border-top-height (+ (* outer-border-width inSymmetric) (* outer-border-top-height (* -1 (- inSymmetric 1)))))
            ;(outer-border-bottom-height outer-border-top-height)
            ;
            ; W-Border and Top-H-Border = % of widht, Bottom-H-Border = (1.41*(TheWidth+2*Border) - Border - TheHeight) (asymmetric)
            ;
            ;(outer-border-top-height (+ (* outer-border-width inPortrait) (* outer-border-top-height (* -1 (- inPortrait 1)))))
            ;(temp-bottom (- (* 1.414214 (+ TheWidth (* 2 outer-border-width))) TheHeight outer-border-top-height))
            ;(outer-border-bottom-height (+ (* temp-bottom inPortrait) (* outer-border-top-height (* -1 (- inPortrait 1)))))
            ;
            ;(inner-border-width (/ (* (+ (* TheHeight inPortrait) (* TheWidth (* -1 (- inPortrait 1)))) ininnerPercent) 100))
            ;
            ;(image-width (+ TheWidth  (* 2 outer-border-width)))
            ;(image-height (+ TheHeight outer-border-bottom-height outer-border-top-height))
            
            (Old-FG-Color (car (gimp-context-get-foreground)))
            (stripelayer 0)
            
            (vertline FALSE)
            (stripeheight 300)
            (stripewidth 50)
            
            (xstart 0)
            (xstep 0)
            (stripe1x 1)
            (stripe1y 1)
            (stripe2x 0)
            (stripe2y 0)
            (numrowsinset 2)
            (newstripeheight 0)
            (newstripestart 0)
            (count 1)
            (count2 0)
            (count3 0)
            (maincount 0)
            (bigset 0)
            (mainjump 0)
            (usecolor '(0 0 0))
            (internalpos 0)
            (TheRowHeight 0)
            (checknum 0)
        )
              
        
        (gimp-image-undo-group-start TheImage)
        (gimp-selection-none TheImage)
        (gimp-message "A")
        (set! stripelayer (car (gimp-layer-new TheImage TheWidth TheHeight RGBA-IMAGE "StripeLayer" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer TheImage stripelayer 0 -1)
        (gimp-message "layer added")
        (gimp-message (number->string stripetype))
        (if (= stripetype 1)
            (begin
                (gimp-message "do the first set method")
                (fu-draw-first-set TheImage TheLayer 
                            stripeno
                            1
                            1
                            TheWidth
                            rowheight
                            FALSE
                            numcolors
                            colorA
                            colorB
                            colorC
                            colorD
                            colorE
                            colorF
                          )
                (gimp-displays-flush)
                (gimp-display-new TheImage)
                (gimp-image-undo-group-end TheImage)
                (quit)
            )
        )
        
        (if (= vertline FALSE)
            (begin
                ; width must be 50 and height 5
                (set! stripewidth TheWidth)
                (set! stripeheight rowheight)
                (set! stripe1x 1)
                (set! stripe1y 1)
                (set! mainjump (+ 0 (* stripeheight numcolors)))
            )
            (begin
                ; else width must be 5 and height 50
                (set! stripewidth rowheight)
                (set! stripeheight TheHeight)
                (set! stripe1x 1)
                (set! stripe1y 1)
                (set! mainjump (* stripewidth (+ 1 numcolors)))
            )
        )
        (set! newstripestart 1)
        (set! maincount 1)
        (set! count 1)
        (set! count2 1)
        (set! count3 1)
        (set! numrowsinset numcolors)
        (set! bigset stripeno)
        
        (while (<= maincount bigset)
            (begin
                (gimp-message "main count")
                (gimp-message (number->string maincount))
                (gimp-message "starting a set")
                
                (set! count 1)
                (while (<= count numrowsinset)
                    (begin
                        (gimp-message "1st color known")
                        (gimp-message (number->string count))
                        (gimp-message "draw pos=1 using color count")
                        
                        (set! count2 1)
                        (while (<= count2 numrowsinset)
                            (begin
                                (if (= count count2)
                                    (begin
                                        (gimp-message "skipping outer one")
                                    )
                                    (begin 
                                        (gimp-message "end colors known")
                                        (gimp-message "draw pos=numrowsinset using color count2")
                                        (gimp-message "starting a set")
                                        (gimp-message (number->string count2))
                                        
                                        (gimp-message (number->string count))
                                        (set! usecolor (determinecolor count    colorA
                                                                colorB
                                                                colorC
                                                                colorD
                                                                colorE
                                                                colorF))
                                        (gimp-message "back from determine")
                                        (gimp-message (number->string stripe1y))
                                        (fu-draw-stripe TheImage stripelayer stripe1x stripe1y stripewidth stripeheight FALSE usecolor 1)
                                        
                                        (gimp-message (number->string count2))
                                        (set! usecolor (determinecolor count2    colorA
                                                                colorB
                                                                colorC
                                                                colorD
                                                                colorE
                                                                colorF))
                                        (gimp-message "back from determine")
                                        (gimp-message (number->string stripe1y))
                                        (fu-draw-stripe TheImage stripelayer stripe1x stripe1y stripewidth stripeheight FALSE usecolor numrowsinset)
                                        
                                        (set! internalpos 2) ; start filling from position 2 to n-1 and use the color determined by the counter
                                        (set! count3 1)
                                        (while (<= count3 numrowsinset)
                                            (begin
                                                (gimp-message "rest of colors known")
                                                (if (or (= count3 count2) (= count3 count))
                                                    (begin
                                                        (gimp-message "skipping inner ones")
                                                    )
                                                    (begin
                                                        (gimp-message "draw pos pos=count3 using color=count3")
                                                        (gimp-message (number->string count3))
                                                        
                                                        (set! usecolor (determinecolor count3    colorA
                                                                colorB
                                                                colorC
                                                                colorD
                                                                colorE
                                                                colorF))
                                                        (gimp-message "back from determine")
                                                        (gimp-message (number->string stripe1y))
                                                        (fu-draw-stripe TheImage stripelayer stripe1x stripe1y stripewidth stripeheight FALSE usecolor internalpos)
                                                        (set! internalpos (+ internalpos 1))
                                                        
                                                        
                                                    )
                                                )
                                                (set! count3 (+ count3 1))
                                                
                                            )
                                        )
                                        (if (= vertline FALSE)
                                            (begin
                                                (set! stripe1y (+ stripe1y mainjump))
                                                (gimp-message "stripey increasing by big jump")
                                                (gimp-message (number->string stripe1y))
                                                ; now check we haven't jumped out the image
                                                (set! checknum (+ stripe1y (* numrowsinset stripeheight)))
                                                (if (> checknum TheHeight)
                                                    (begin
                                                        (gimp-message "height exceeded. stop loops")
                                                        (set! count3 (+ numrowsinset 1))
                                                        (set! count2 (+ numrowsinset 1))
                                                        (set! count (+ numrowsinset 1))
                                                        (set! maincount (+ bigset 1))
                                                    )
                                                )
                                            )
                                            ;; (set! stripe1x (+ stripe1x mainjump))
                                        )
                                        (gimp-message "first group of 5 DONE")
                                        (gimp-displays-flush)
                                        (gimp-display-new TheImage)
                                    )
                                )                              
                                (set! count2 (+ count2 1))
                            )
                            
                        )
                        (set! count (+ count 1))
                    )
                )
                
                (set! maincount (+ maincount 1))
            )
        )
        (gimp-message "FINAL QUIT")
        ;;(fu-draw-stripe TheImage stripelayer 1 325 stripewidth stripeheight FALSE '(100 100 100) 1)
        
        
            
            
                    
        ;(gimp-image-select-rectangle TheImage CHANNEL-OP-ADD 1 (+ (* stripeheight 6) 1) stripewidth stripeheight)
        ;(gimp-context-set-foreground colorA)
        ;(gimp-edit-fill stripelayer FOREGROUND-FILL)
        
        
        ;(gimp-item-set-name TheLayer "WithBorder")
        ;(gimp-image-insert-layer TheImage BorderLayer 0 -1)
        ;    (gimp-edit-clear BorderLayer)
        ;    ;
        ;    (gimp-context-set-feather FALSE)
        ;    ;
        ;    (gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE 0 0 image-width image-height)
        ;    (gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
        ;    (gimp-context-set-foreground inOuterColor)
        ;    (gimp-edit-fill BorderLayer FOREGROUND-FILL) 
        ;    ;
        ;    (cond
        ;        ((> ininnerPercent 0) 
        ;            (begin
        ;                (gimp-context-set-feather inFeather)
        ;                (gimp-context-set-feather-radius (* 1.4 inner-border-width) (* 1.4 inner-border-width))
        ;                (gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE (- outer-border-width inner-border-width) (- outer-border-top-height inner-border-width) (+ TheWidth (* inner-border-width 2)) (+ TheHeight (* inner-border-width 2)))
        ;                (gimp-context-set-feather FALSE)
        ;                (gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
        ;                (gimp-context-set-foreground ininnerColor)
        ;                (gimp-edit-fill BorderLayer FOREGROUND-FILL)
       ;             )
       ;         )
        ;    )
        ;    (gimp-image-merge-down TheImage BorderLayer CLIP-TO-IMAGE)
    
    ;
    ;(gimp-selection-none TheImage)
    (gimp-displays-flush)
    (gimp-display-new TheImage)
    (gimp-image-undo-group-end TheImage)
  ) ;;; let
  ;
  ; Finish work
  ; 
  ;
    
)
 
; Register the function with the GIMP

(script-fu-register
   "script-fu-stripes"
   "<Image>/Script-Fu2/Render/Stripes..."
   "Render some stripes. Pattern has large repeat cycle. Doesn't fill the full image as this may impact the repeatability. \nfile:karlhof26-stripes-03.scm"
   "Karl Hofmeyr email"
   "Karl Hofmeyr, city country"
   "2016"
   "RGB* GRAY*"
   SF-IMAGE "The Image" 0
   SF-DRAWABLE "The Layer" 0
   SF-ADJUSTMENT "Number of stripe sets - only if very large image" '(1 1 5 1 0 0 0)
   SF-OPTION "Type Stripe"  '("Rythmic Bells Pseudo" "First set" "Shades")
   SF-ADJUSTMENT "Number of Colors"     '(4 3 6 1 0 0 0)
   SF-ADJUSTMENT "Row Height"           '(10 1 50 1 0 0 0)
   ;;SF-TOGGLE "Use symmetric outer borders" FALSE
   ;;SF-TOGGLE "Use portrait style border" FALSE
   ;;SF-ADJUSTMENT "inner border size -in percent" '(0.20 0.0 10.0 0.1 0 2 0)
   ;; SF-COLOR "Outer border color" '(255 255 255)
   SF-COLOR "color 1" '(0 0 0)
   SF-COLOR "color 2" '(200 50 50)
   SF-COLOR "color 3" '(50 200 50)
   SF-COLOR "color 4" '(50 50 200)
   SF-COLOR "color 5" '(200 200 50)
   SF-COLOR "color 6" '(200 50 200)
   ;;SF-TOGGLE "Feather inner border" FALSE
)
;

;
; end of script
