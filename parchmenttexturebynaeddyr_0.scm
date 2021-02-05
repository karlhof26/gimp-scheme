; Copyright 2009-2011 Kristian Järventaus
; version 2.0 of my paper texture script, renamed Parchment Texture Script
; Licensed under Creative Commons Attribution Share-alike license 3.0
; Disclaimer: I've studied programming since I was a little kid, mostly reading books and maybe doing a small program in whatever flavour of language I was interested in...
; The disclaimer part is this: even though I'd study a language (ranging from Basic to Pascal in school to Inform 6 which was a C-syntax-y Z-engine language, and I'm Pythoning at the moment)
; I never used them to actually program, so I have very, very little actual experience. The results can be seen below. I know, I know. If I could bother with it, I'd encapsulate
; all the steps into separate functions, but eh.
; If you find a bug (and you will), please contact me. 



(define (script-fu-parchment-texture inImage inLayer inFlatten inContrast inColour inCartoon inCartoonStr inBiteStrength inBiteGamma inLava)
    (let* ( 
            (theImage 0) ; the target image
            (originalLayer 0) ; original layer, a copy because i couldn't be bothere to change all the code that uses "theLayer" (which is Plugin>Cartoon'd)
            (blurredLayer 0) ; blurred original layer
            (theLayer 0) ; the base layer, the original image's copy
            (theScrunch 0) ; embossed noise layer used to make poxy texture
            (theBite 0) ; white spot layer used to make printing faults
            (biteMask 0)
            (multiYella 0) ; yellowing (or any colour) layer with multiply
            (colorYella 0) ; yellowing layer with coloring
            (theVertical 0) ; vertical fibres
            (theHorizontal 0) ; horizontal fibres
            (thePlasma 0) ; badgering layer on top
            (theGrungeLayer 0) ; aging layer
                
                (lavaLayer 0) ; layer with squiggly lines
                (edgeMask 0) ; layer for the the edge grunge layer
                (secondGrungeLayer 0) ; light grunge through the whole image
                (theGrungeMask 0) ; mask of the secondary grunge layer filled with plasma, and used as the base of the grunge layer drawing
                (speckleLayer 0) ; paper or skin speckles you see on old parchments. I think they're birth marks of the animal.
                (theWidth 0) ; width of the image
                (theHeight 0) ; height of the image
                (theContrast 4.1) ; i don't remember what this was for, so it can't be that important
                (thatDamnFloater 0) ; sounds like this one has some sort of history but i can't remember what it was; comment added at a much later update
                (theIterator 0) ; should just have used local blocks with i
                (perimeter 0) ; this is the perimeter of the image
                (area 0) ; area of the image
                (inFibreOpacity 15)
                (randomPoints (cons-array 2 'double))
                
                (karlmaster1 0)
           ) ; should have use a local (let block for these, an old-style array ("vector" in moderner scheme dialects) for x,y coordinates
             ; note the un-scheme-like variable names. should just have use dashes.
            
            
            
            
            
        (gimp-image-undo-group-start inImage) ; the undo block
        
        ;(gimp-message "line53")
        (set! theImage (car (gimp-image-duplicate inImage)))
        (set! theLayer (car (gimp-image-flatten theImage)))
        (if (not (= (car (gimp-image-base-type theImage)) 0))
            (gimp-image-convert-rgb theImage))
        (gimp-layer-set-name theLayer "Cartooned layer copy")
        (set! theWidth (car (gimp-image-width theImage)))
        (set! theHeight (car (gimp-image-height theImage)))
        (set! area (* theWidth theHeight))
        (set! perimeter (* 2 (+ theWidth theHeight)))
        
        
        (set! originalLayer (car (gimp-layer-copy theLayer FALSE)))
        ;(gimp-message "line66")
        (gimp-layer-set-name originalLayer "Original background layer")
        (set! blurredLayer (car (gimp-layer-copy theLayer FALSE)))
        (gimp-layer-set-name blurredLayer "Blurred layer copy")
        (gimp-image-add-layer theImage originalLayer -1)
        (gimp-layer-set-opacity originalLayer 70)
        
        (gimp-image-add-layer theImage blurredLayer -1)
        (gimp-layer-set-opacity blurredLayer 40)		
        (plug-in-blur 1 theImage blurredLayer)
        ;(gimp-message "line76")
        
        (if (= inCartoon TRUE) ; the "enlivening" procress, toggle widget
            (begin
                (plug-in-cartoon 1 theImage theLayer inCartoonStr 0.2)      ; cartoon makes for a nice "print" look; was 0.0 for pctBlack
                (plug-in-sharpen 1 theImage theLayer 30)        ; sharpening takes just a teensy bit off the edges of letter forms
                (gimp-image-set-active-layer theImage blurredLayer)	          
                (set! theBite (car 	                    ; the white splotches, representing sloppy printing and wear-and-tear
                        (gimp-layer-new
                            theImage
                            theWidth
                            theHeight
                            RGB-IMAGE
                            "White bite (control with Color > Level)"
                            60       ;"Bite opacity", the layer opacity
                            LAYER-MODE-ADDITION)))
                ;(gimp-message "line92")
                (gimp-image-insert-layer theImage theBite 0 -1)
                (gimp-context-set-background '(0 0 0))
                (gimp-edit-bucket-fill theBite BUCKET-FILL-BG 0 100 0 FALSE 1 0)      ; is there a simpler way to do this?
                (plug-in-hsv-noise 1 theImage theBite 1 0 0 255)    ; white noise
                (plug-in-blur 1 theImage theBite)       ; blur so that the Levels tool has something to work with
                ;(gimp-message "line98")
                (gimp-levels    theBite
                        0 ;channel: value
                        0 ;black input
                        (- 160 inBiteStrength)          ; white input, "Bite strength". the dialog lets you choose between 0 and 20, which is then here adjusted with MATHS
                        (/ (+ 20 inBiteGamma) 100)          ; gamma, "Bite spread", also in the dialog.
                        0 ; intensity black, default
                        255)
                (set! biteMask (car (gimp-layer-create-mask theBite ADD-MASK-WHITE)))
                (gimp-layer-add-mask theBite biteMask)
                
                ; karlhof26 - plasma has a bug - result is displaced
                ;(plug-in-plasma 1 theImage biteMask (rand 10000) 7.0)  
                ;(plug-in-hsv-noise 1 theImage biteMask 5 30 145 90) ; was 7 20 125 40
                ;(plug-in-randomize-pick 1 theImage biteMask 58.2 12.2 TRUE 198567) 
                (plug-in-solid-noise 1 theImage biteMask FALSE TRUE 1234567 5 3.2 8.1)
            )
        )
        ;(gimp-message "line116")
        ;(gimp-displays-flush)
        ;(gimp-display-new theImage)
        ;(gimp-message "Early quit - line 119")
        ;(quit)
        
        (gimp-image-set-active-layer theImage blurredLayer)
        (gimp-image-set-active-layer theImage theBite)             
        (set! speckleLayer (car (gimp-layer-new theImage
                                theWidth
                                theHeight
                                RGB-IMAGE
                                "Parchment speckles (remove excess)"
                                60
                                LAYER-MODE-NORMAL)))
        (gimp-drawable-fill speckleLayer FILL-WHITE)
        (gimp-image-insert-layer theImage speckleLayer 0 -1)
        (gimp-message "line 133")
        
        (let* ( (speckler 0)
                (i 0)
                (pt1x 1)
                (pt2x 2)
                (pt1y 1)
                (pt2y 2)
               )
               
            (if (= (car (gimp-brushes-get-list "^speckler$")) 0)
                (set! speckler (car (gimp-brush-new "speckler")))
                (set! speckler "speckler")
            )
            (gimp-message "line147")
            
            (gimp-brush-set-hardness speckler 0.60)
            (gimp-brush-set-radius speckler 3.0)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-context-set-brush speckler)
            
            ;(gimp-by-color-select originalLayer '(255 255 255) 15 0 TRUE FALSE 0 FALSE)
            (gimp-context-set-sample-threshold 0.25)
            (gimp-image-select-color theImage CHANNEL-OP-ADD originalLayer '(255 255 255))
            (gimp-progress-init "speckle" -1)
            (while (< i (/ area 30000))            ; was 50k
                (set! pt1x (round (rand theWidth)))
                (set! pt2x (- pt1x 3))
                (set! pt1y (round (rand theHeight)))
                (set! pt2y (- pt1y 3))
                
                (gimp-brush-set-radius speckler (/ (+ 10 (rand 20)) 3)) ; was divide 10
                
                (gimp-paintbrush-default speckleLayer 4 (vector pt1x pt1y pt2x pt2y ))
                
                (set! i (+ i 1))
                ;(gimp-message "speckle dab")
                ;(gimp-message (number->string (rand theWidth)))
                ;(gimp-message (number->string (/ (+ 10 (rand 20)) 3)))
                ;(gimp-message (number->string i))
                (gimp-progress-update (/ i (/ area 30000)))
            )
            (gimp-selection-none theImage)
        )
        
        (plug-in-colortoalpha 1 theImage speckleLayer '(255 255 255))
        
        ;(gimp-display-new theImage)
        (gimp-message "line 181")
        ;(quit)
        
            (if (= inLava TRUE)
                (begin
                    (gimp-message "line151")
                    (set! lavaLayer (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGB-IMAGE
                                        "Lava layer for squiggly lines"
                                        12
                                        LAYER-MODE-OVERLAY-LEGACY))) ; was normal
                    (gimp-image-insert-layer theImage lavaLayer 0 -1)
                    ;(print "before lava")
                    (gimp-selection-all theImage)
                    (script-fu-lava theImage lavaLayer (round (rand 8000)) 16.1 7.2 "Browns" TRUE TRUE FALSE) ; was FFF at end
                    (gimp-selection-none theImage)
                    
                    (gimp-display-new theImage)
                    ;(quit)
                    
                    ;(set! lavaLayer (car (gimp-image-get-layer-by-name theImage "Lava Layer")))
                    ;(gimp-image-set-active-layer theImage (car (gimp-image-get-layer-by-name theImage "Lava Layer")))
                    (gimp-image-merge-down theImage (car (gimp-image-get-layer-by-name theImage "Lava Layer")) EXPAND-AS-NECESSARY)
                    (set! lavaLayer (car (gimp-image-get-layer-by-name theImage "Lava layer for squiggly lines")))
                    
                    (gimp-message "line208")
                    (gimp-drawable-invert lavaLayer TRUE) 
                    
                    
                    (gimp-drawable-desaturate lavaLayer DESATURATE-LIGHTNESS)
                    (gimp-drawable-levels lavaLayer HISTOGRAM-VALUE 0.85 1.0 TRUE 1.0 0.0 1.0 TRUE)
                    (gimp-message "line214")
                    
                    
                    (plug-in-colortoalpha 1 theImage lavaLayer '(255 255 255))
                    (gimp-layer-set-mode lavaLayer LAYER-MODE-MULTIPLY) ; was overlay
                    (gimp-layer-set-opacity lavaLayer 48) ; was 32)
                    (gimp-message "line220")
                    (gimp-display-new theImage)
                    
                    
                )
            )
        (gimp-message "line226")
        
        ;(gimp-display-new theImage)
        ;(gimp-message "Early quit - line 229")
        ;(quit)
        
        (set! theGrungeLayer (car (gimp-layer-new theImage
                            theWidth
                            theHeight
                            RGB-IMAGE
                            "Edge grunge (edit mask with Blend)"
                            100
                            LAYER-MODE-GRAIN-MERGE-LEGACY)))
        
        (gimp-drawable-fill theGrungeLayer FILL-WHITE)
        (gimp-image-insert-layer theImage theGrungeLayer 0 -1)
        (set! edgeMask (car (gimp-layer-create-mask theGrungeLayer 0)))
        (gimp-layer-add-mask theGrungeLayer edgeMask)
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (gimp-context-set-opacity 100)
        (gimp-edit-blend edgeMask BLEND-FG-BG-RGB LAYER-MODE-NORMAL-LEGACY GRADIENT-RADIAL 100 60 REPEAT-NONE FALSE FALSE 1 0 TRUE (/ theWidth 2) (/ theHeight 2) 0 0)
        
        ;(gimp-display-new theImage)
        ;(gimp-message "Early quit - line 251")
        ;(quit)
        
        (gimp-context-set-brush "Galaxy, Big")
        (gimp-message "line255")
        ;(gimp-displays-flush)
        ;(gimp-display-new theImage)
        
        
        (if (and (>= theWidth 100) (>= theHeight 100))
            (begin
                (gimp-message "line262")
                (gimp-context-set-feather TRUE)
                (gimp-context-set-feather-radius 50.0 50.0)
                ;;rmvd karlhof26;;(gimp-round-rect-select theImage 40 40 (- theWidth 80) (- theHeight 80) 200 200 0 FALSE TRUE 50 50)
                (gimp-image-select-round-rectangle theImage CHANNEL-OP-REPLACE 40 40 (- theWidth 80) (- theHeight 80) 100 100)
                (if (and (> theWidth 301) (> theHeight 301))
                    (begin
                        (gimp-message "bigger image")
                        ;;(gimp-round-rect-select theImage 100 100 (- theWidth 200) (- theHeight 200) 150 150 1 FALSE FALSE 0 0)
                        (gimp-image-select-round-rectangle theImage CHANNEL-OP-REPLACE 80 80 (- theWidth 160) (- theHeight 160) 120 120)
                    )
                )
                ;(gimp-message "line267")
                
                (let* ( (i1 0)
                        (i2 0)
                        (pointList '())
                        (randomColor 0)
                        (xy (make-vector 2 'double))
                       )
                    (gimp-message "line282")
                    (set! karlmaster1 400) ; was 1200 set back once finished testing
                    (gimp-progress-init "adding1" 0)
                    
                    (while (and (< i1 (/ perimeter 5)) (> karlmaster1 1))
                        (let* (
                                (sector 0)
                                (margin 70)
                              )
                            (set! sector (rand 4))
                            ;random xy-coordinates, but not in the center, faster this way than rejecting random coordinates from the whole area,
                            ;especially as the image becomes bigger
                            (cond ((= sector 0)
                                    (begin
                                        (gimp-message "line296")
                                        (vector-set! xy 0 (round (rand (- theWidth 5))))
                                        (vector-set! xy 1 (round (rand margin)))
                                    )
                                   )
                                ((= sector 1)
                                    (begin
                                        (gimp-message "line303")
                                        (vector-set! xy 0 (round (rand (- theWidth 5))))
                                        (vector-set! xy 1 (round (+ (rand margin) (- (- theHeight 5) margin))))
                                    )
                                )
                                ((= sector 2)
                                    (begin
                                        (gimp-message "line310")
                                        (vector-set! xy 0 (round (rand margin)))
                                        (vector-set! xy 1 (round (rand (- theHeight 5))))
                                    )
                                )
                                ((= sector 3)
                                    (begin
                                        (gimp-message "line317")
                                        (vector-set! xy 0 (round (+ (rand margin) (- (- theWidth 5) margin))))
                                        (vector-set! xy 1 (round (rand (- theHeight 3))))
                                    )
                                )
                            )
                        )
                        
                            ;(gimp-display-new theImage)
                            ;(gimp-message "Early quit - line 326")
                            ;(quit)
                            
                            (gimp-message "line329")
                            ;(gimp-message (number->string (round (random theWidth))))
                            ;(vector-set! xy 0 (round (random theWidth)))
                            ;(gimp-message "line254")
                            ;(gimp-message (number->string (vector-ref xy 0)))
                            ;(vector-set! xy 1 (round (random theHeight)))
                            (gimp-message "line335")
                            (gimp-message (number->string (car (gimp-selection-value theImage (vector-ref xy 0) (vector-ref xy 1)))))
                            (gimp-message "line337")
                            
                            (if (< (car (gimp-selection-value theImage (vector-ref xy 0) (vector-ref xy 1))) (round (rand 150)))
                                (begin 
                                    (gimp-message "line341 ADDDING")
                                    (set! i1 (+ i1 1))
                                    ;(print i1)
                                    (set! pointList (cons (vector (vector-ref xy 0) (vector-ref xy 1)) pointList))
                                )
                                (begin
                                    (gimp-message "line339 NOT Adding 00000")
                                    (set! karlmaster1 (- karlmaster1 1))
                                    ;(gimp-message (number->string karlmaster1))
                                )
                            )
                            ;(gimp-message "line344")
                            ;(gimp-message (number->string i1))
                            ;(gimp-message (number->string (/ i1 (/ perimeter 5))))
                            (gimp-progress-update (/ i1 (/ perimeter 5)))
                            
                            
                        
                    )
                    
                    (gimp-message "line353")
                    (gimp-message "Is final il odd or even?")
                    (if (= (abs i1 2) 0 )
                            (begin
                                ;(gimp-message "line357")
                                (set! i1 (- i1 1))
                            )
                    )
                    
                    ;(gimp-message "line362 out first Adding1 while")
                    (gimp-selection-none theImage)
                    
                    ;(print pointList)
                    ;(gimp-message "line366")
                    ;(gimp-message (number->string i1))
                    ;(gimp-message (number->string i2))
                            
                    (gimp-progress-init "painting1" -1)
                    (while (and (< i2 (/ perimeter 10)) (< i2 i1))
                        ;(gimp-message "line372")
                        (set! randomColor (round (rand 128)))
                        (gimp-context-set-foreground (list randomColor randomColor randomColor))
                        (gimp-paintbrush-default theGrungeLayer 2 (car pointList))
                        (set! pointList (cdr pointList))
                        (set! i2 (+ i2 1))
                        (gimp-progress-update (/ i2 (/ perimeter 10)))
                    )
                )
                ;(gimp-message "line381")
                ;(gimp-display-new theImage)
                ;(quit)
                
                (gimp-context-set-feather FALSE)
                (gimp-context-set-feather-radius 0.0 0.0)
                
                ;;(gimp-round-rect-select theImage 20 20 (- theWidth 40) (- theHeight 40) 150 150 0 FALSE FALSE 0 0)
                (gimp-image-select-round-rectangle theImage CHANNEL-OP-REPLACE 20 20 (- theWidth 40) (- theHeight 40) 95 95) ; was 150 150 at end but can't confirm width is bigger than 100.
                (if (and (> theWidth 301) (> theHeight 301))
                    (begin
                        ;(gimp-message "bigger image")
                        ;;(gimp-round-rect-select theImage 100 100 (- theWidth 200) (- theHeight 200) 150 150 1 FALSE FALSE 0 0)
                        (gimp-image-select-round-rectangle theImage CHANNEL-OP-REPLACE 80 80 (- theWidth 160) (- theHeight 160) 120 120)
                    )
                )
                (if (and (> theWidth 601) (> theHeight 601))
                    (begin
                        ;(gimp-message "Even bigger image")
                        ;;(gimp-round-rect-select theImage 100 100 (- theWidth 200) (- theHeight 200) 150 150 1 FALSE FALSE 0 0)
                        (gimp-image-select-round-rectangle theImage CHANNEL-OP-REPLACE 180 180 (- theWidth 380) (- theHeight 380) 170 170)
                    )
                )
                
                (let* (
                        (i1 0)
                        (i2 0)
                        (pointList '())
                        (xy (make-vector 2 'double))
                      )
                    (gimp-progress-init "adding2" -1)
                    
                    (set! pointList (cons (vector 45 45) (cons (vector 45 (- theHeight 45)) (cons (vector (- theWidth 45) 45) (cons (vector (- theWidth 45) (- theHeight 45)) (vector 0) )))))			
                    (while (< i1 (/ perimeter 15))
                            ;(gimp-message "line415")
                            (vector-set! xy 0 (round (rand theWidth)))
                            (vector-set! xy 1 (round (rand theHeight)))
                            (if (> (car (gimp-selection-value theImage (vector-ref xy 0) (vector-ref xy 1))) 0)
                                (begin
                                    (set! i1 (+ i1 1))
                                    ;(print i1)
                                    (set! pointList (cons (vector (vector-ref xy 0) (vector-ref xy 1)) pointList))
                                )
                                (begin
                                    ;(gimp-message "Discard item")
                                )
                            )
                           ;(gimp-message (number->string (/ i1 (/ perimeter 20))))
                           (gimp-progress-update (/ i1 (/ perimeter 15)))
                    )
                    ;(gimp-message "line431 out of while adding2")
                    ;(gimp-display-new theImage)
                    ;(quit)
                    
                    (gimp-selection-none theImage)
                    
                    ;(print pointList)
                    (gimp-context-set-opacity 70)
                    (gimp-context-set-foreground '(255 255 255))
                    ;(gimp-message "line440")
                    (gimp-progress-init "painting2" -1)
                    (while (< i2 (+ (/ perimeter 20) 4)) ; (the perimeter constant is used to determine how much edge grunge there will be, and the + 4 is for
                                              ; the set vectors above that are used to cut the corners in.
                            (gimp-paintbrush-default theGrungeLayer 2 (car pointList))
                            (set! pointList (cdr pointList))
                            (set! i2 (+ i2 1))
                            (gimp-progress-update (/ i2 (+ (/ perimeter 20) 4)))
                    )
                    ;(gimp-message "out of painting2 while")
                    
                )
                (plug-in-colortoalpha 1 theImage theGrungeLayer '(255 255 255))
                (gimp-context-set-opacity 100)
                ;(gimp-display-new theImage)
            )
        )
        
        (gimp-message "line458")
        (gimp-display-new theImage)
        ;(quit)
        
        (set! secondGrungeLayer (car (gimp-layer-new  theImage
                                theWidth
                                theHeight
                                RGB-IMAGE
                                "Light grunge (edit mask with Level)"
                                20
                                LAYER-MODE-NORMAL)))        ; was normal
        (gimp-drawable-fill secondGrungeLayer FILL-WHITE)
        (gimp-image-insert-layer theImage secondGrungeLayer 0 -1)
        (set! theGrungeMask (car (gimp-layer-create-mask secondGrungeLayer 0)))
        (gimp-layer-add-mask secondGrungeLayer theGrungeMask)
        
        (gimp-message "line474 plasma here")
        (gimp-display-new theImage)
        ;(quit)
        
        ;(plug-in-plasma 1 theImage theGrungeMask (rand 10000) 5.0)
        ;(plug-in-hsv-noise 1 theImage biteMask 5 30 145 90) ; was 7 20 125 40
        ;(plug-in-randomize-pick 1 theImage biteMask 58.2 12.2 TRUE 198567) 
        (plug-in-solid-noise 1 theImage biteMask FALSE TRUE (rand 10000) 12 3.3 2.1) ;was 7 5.3 4.1
                
        (let* (
                (i 0)
                (randomColor 0)
                (randomx 0)
                (randomy 0)
                
                (karlmax 100)
              )
              
            (gimp-message "line490")
            (gimp-display-new theImage)
            (gimp-progress-init "painting3" 0)
            (while (and (< i (/ area 8000)) (> karlmax 1)) ; was 4000
                (set! randomx (- (rand theWidth) 1))
                (set! randomy (- (rand theHeight) 1))
                ;(gimp-message (number->string randomx))
                ;(gimp-message (number->string randomy))
                ;(gimp-message (number->string (car (gimp-drawable-get-pixel theLayer randomx randomy))))
                ;(gimp-message (number->string (car (gimp-drawable-get-pixel theGrungeMask randomx randomy))))
                ;(gimp-message (number->string (vector-ref (car (cdr (gimp-drawable-get-pixel theGrungeMask randomx randomy))) 0) ))
                ;(gimp-message (number->string (/ i (/ area 4000))))
                (if (> (vector-ref (car (cdr (gimp-drawable-get-pixel theGrungeMask randomx randomy))) 0) (round (rand 20)))
                ;(if (> (car (gimp-selection-value theImage randomx randomy)) (round (rand 20)))
                ; was was rand 200 at end
                    (begin
                        (set! randomColor (round (rand 254)))
                        (gimp-context-set-opacity (+ 59 (round (rand 40))))
                        (gimp-context-set-foreground (list randomColor randomColor randomColor))
                        (gimp-paintbrush-default secondGrungeLayer 2 (vector randomx randomy))
                        (set! i (+ i 1))
                        
                        
                    )
                    (begin
                        (set! karlmax (- karlmax 1))
                        (set! i (+ i 1))
                        (gimp-message "karlmax changed")
                    )
                )
                (gimp-progress-update (/ i (/ area 8000)))
            )
        )
        
        (gimp-message "line513")
        (plug-in-colortoalpha 1 theImage secondGrungeLayer '(255 255 255))
        (plug-in-blur 1 theImage theGrungeMask)
        (gimp-layer-set-mode secondGrungeLayer LAYER-MODE-DARKEN-ONLY)
        
        ;(gimp-message "line428")
        ;(gimp-display-new theImage)
                    
        (set! theScrunch (car (gimp-layer-new   theImage        ;the embossed noise layer, poxy, porous look
                                            theWidth
                                            theHeight
                                            RGB-IMAGE
                                            "Pore texture"
                                            5
                                            LAYER-MODE-HSV-VALUE)))
        
        (gimp-drawable-fill theScrunch FILL-WHITE)
        (gimp-image-insert-layer theImage theScrunch 0 -1)
        (plug-in-hsv-noise 1 theImage theScrunch 1 0 100 0)
        (plug-in-emboss 1 theImage theScrunch 15 50 5 1)
        
        (set! multiYella (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGB-IMAGE
                                        "Multiply with age (control with opacity)"
                                        3 ; how yella do ya want it
                                        LAYER-MODE-MULTIPLY-LEGACY)))
        
        (gimp-image-insert-layer theImage multiYella 0 -1)
        (gimp-context-set-background inColour)      ; brown works too.
        (gimp-edit-bucket-fill multiYella 1 0 100 0 FALSE TRUE 0)
        
        (set! colorYella (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGB-IMAGE
                                        "Colourise with age (control with opacity)"
                                        33 ; 
                                       LAYER-MODE-HSL-COLOR)))
        ;(gimp-message "line553")
        ;(gimp-display-new theImage)
        
        (gimp-image-insert-layer theImage colorYella 0 -1)
        (gimp-context-set-background inColour)     
        (gimp-edit-bucket-fill colorYella BUCKET-FILL-BG 0 100 0 FALSE 1 0)
        
        (set! theHorizontal (car (gimp-layer-new
                                theImage
                                theWidth
                                theHeight
                                RGB-IMAGE
                                "Horizontal stripes"
                                inFibreOpacity
                                LAYER-MODE-LINEAR-BURN)))
        (gimp-drawable-fill theHorizontal FILL-WHITE)
        (gimp-image-insert-layer theImage theHorizontal 0 -1)
        (plug-in-randomize-hurl 1 theImage theHorizontal 100 1 TRUE 10)
        (gimp-drawable-desaturate theHorizontal DESATURATE-LUMINANCE)
        (plug-in-wind 1 theImage theHorizontal 5 0 25 0 1) ; was 25 0 3 at end
        (plug-in-blur 1 theImage theHorizontal)
        ;(gimp-brightness-contrast theHorizontal 0 30)
        (gimp-drawable-brightness-contrast theHorizontal 0.0 0.117)
        ;(gimp-message "line576")
        ;(gimp-display-new theImage)
        
        (gimp-image-rotate theImage 0) ; originally the method was just to duplicate the horizontal layer and rotate the duplicate, but that only works with square images. For ages I tried to figure out how to do rectangular ones, but my attempts at enlarging the images and layers were unsuccesful. And by unsuccessful, I mean buggy, because enlarging those images and layers seems to work, but no plugin works in the "outside" areas and the rotation is centered on the original area, not the new square one, etc. Then lightning struck me, and I felt really stupid, and just did this: rotate the fricking image, do the Vertical layer, then rotate it back.
        
        (set! theVertical (car (gimp-layer-new
                            theImage
                            theHeight
                            theWidth
                            RGB-IMAGE
                            "Vertical stripes"
                            inFibreOpacity
                            LAYER-MODE-BURN)))
        
        (gimp-drawable-fill theVertical FILL-WHITE)
        (gimp-image-insert-layer theImage theVertical 0 -1)
        (plug-in-randomize-hurl 1 theImage theVertical 100 1 TRUE 10)
        (gimp-drawable-desaturate theVertical DESATURATE-LUMINANCE)
        (plug-in-wind 1 theImage theVertical 15 0 25 0 0) ; was 5 0 25 0 3 at end 
        (plug-in-blur 1 theImage theVertical)
        (gimp-drawable-brightness-contrast theVertical 0.0 0.117) ; 0 30
        ;(gimp-message "line597")
        ;(gimp-display-new theImage)
        
        (gimp-image-rotate theImage 2)
        
        (set! thePlasma (car (gimp-layer-new ; Splotches of dark and light to create an aged look.
                    theImage
                    theWidth
                    theHeight
                    RGB-IMAGE
                    "Plasma layer (control with opacity)"
                    30
                    LAYER-MODE-OVERLAY-LEGACY)))
        (gimp-image-insert-layer theImage thePlasma 0 -1)
        ;(set! theContrast 4.1)
        
        ;(plug-in-plasma 1 theImage thePlasma (rand 5000) inContrast)
        (plug-in-solid-noise 1 theImage biteMask FALSE TRUE (rand 10000) 10 5.3 3.1)
        (gimp-drawable-desaturate thePlasma DESATURATE-LUMINANCE)
        
        
        (if (= inFlatten TRUE)
            (begin
                (gimp-image-flatten theImage)
            )
        )
        (gimp-image-clean-all theImage)
        (gimp-display-new theImage)
        (gimp-selection-none inImage)
        (gimp-selection-none theImage)
        ;(gimp-message "line619")
        (gimp-message "Good finish OK")
        
        (gimp-displays-flush theImage)
        (gimp-image-undo-group-end inImage)
    )
)

(script-fu-register "script-fu-parchment-texture"
                    "Parchment Texture"
                    "Make an image look like its on old parchment. Parchment texture by Eddy. Works best on pure Black and White Image. Or black text on white background. Long running slow script. \nfile:parchmenttexturebynaeddyr_0.scm"
                    "Kristian Järventaus"
                    "2011, Kristian Järventaus."
                    "June 25th 2011"
                    "RGB* GRAY* INDEX*"
                    SF-IMAGE        "The image"    0
                    SF-DRAWABLE     "The layer"     0
                    SF-TOGGLE       "Flatten"       FALSE
                    SF-ADJUSTMENT   "Plasma wear turbulence"    '(4 1 7 1 3 1 0)
                    SF-COLOR        " Aging color"              '(255 182 0)
                    SF-TOGGLE       "Badger the original image" TRUE
                    SF-ADJUSTMENT   " Print smudge strength (Cartoon plugin)"   '(28 1 40 1 5 1 0)
                    SF-ADJUSTMENT   " Strength of bite"                         '(10 0 40 1 5 0 0)
                    SF-ADJUSTMENT   "  Bite spread"                             '(10 0 20 1 5 0 0)
                    SF-TOGGLE       "Squiggly lines? (Uses the Lava plugin)"     FALSE
)

(script-fu-menu-register  "script-fu-parchment-texture"
                         "<Toolbox>/Script-Fu/Decor/")

; end of script