; 3D Fun rel 0.03  
; Created by Graechan
; You will need to install GMIC to run this Scipt
; GMIC can be downloaded from http://sourceforge.net/projects/gmic/files/ 
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
; Rel 0.02 - Added more text adjustments and did some bugfixes now Script has no dependancies except Gmic
; Rel 0.03 - Added 'Bkg Animation Direction' choice
; Rel 0.04 - Updated to work with latest GMIC - extra Main text color option added.

(define (script-fu-3d-fun image drawable 
                                    bkg-type 
                                    pattern
                                    gradient 
                                    shapes
                                    s-density
                                    s-size
                                    3d-fgd-height
                                    3d-smoothness
                                    frames
                                    direction
                                    texture
                                    txt-color
                                    txt-pattern
                                    txt-gradient
                                    main-text-color
    )
    
    ;; Start undo group.
    (gimp-image-undo-group-start image)
    
    (let* (	
            ;; verification of the dimensions
            (error 0)
            (text-layer (car (gimp-image-get-active-layer image)))
            (width (car (gimp-drawable-width text-layer))) 
            (height (car (gimp-drawable-height text-layer))) 
            (bkg-layer 0)
            
            (shape 0)
            (density 12)
            (size 8.2)      
            (3d_bkg)
            
            (3D_fgd)
            (txt-width 5)
            (txt-height 10)
            (texture-layer 0) 
            (activeLayer 0)
            (temp-layer 0)
            (3d-height 12)
            
          )
          
        ;(gimp-message "start of main prog")
        (gimp-context-push)
        
        (if (< width 24)
            (set! error (+ error 1))
        )
        (if (< height 24)
            (set! error (+ error 10))
        )
        
        (if (= error 1)
            (gimp-message "ERROR 3d_fun WIDTH<24")
        )
        (if (= error 10)
            (gimp-message "ERROR 3d_fun HEIGHT<24")
        )
        (if (= error 11)
            (gimp-message "ERROR 3d_fun WIDTH<24 & HEIGHT<24")
        )
        
        (gimp-context-set-foreground '(0 0 0))
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 1)
        (gimp-drawable-fill bkg-layer FILL-FOREGROUND)
        
        (if (= bkg-type 2);Pattern
            (begin
                ;(gimp-message "pattern fill")
                (gimp-context-set-pattern pattern);;test
                (gimp-drawable-fill bkg-layer FILL-PATTERN)
            )
        )
        (if (= bkg-type 1);;White
            (begin
                ;(gimp-message "white fill")
                (gimp-context-set-background '(255 255 255))
                (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
            )
        )
        
        (if (= bkg-type 3);;Gradient
            (begin
                (gimp-context-set-background '(255 255 255))
                (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
                (gimp-context-set-gradient gradient)    
                (gimp-edit-blend bkg-layer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 0 0 width height);; GRADIENT-LINEAR
            )
        )
        
        ; *******************************************start 3D_bkg
        ;(gimp-display-new image)
        ;(gimp-displays-flush)
        ;(quit)
        
        
        
        
            (set! 3d_bkg (car (gimp-layer-copy bkg-layer TRUE)))
            (set! density s-density)
            (set! size s-size)
            ;(gimp-message "start of 3d bkg")
            ;; Add a layer
            (gimp-image-insert-layer image 3d_bkg 0 1)
            (gimp-image-raise-layer-to-top image 3d_bkg)
            
            ;; name the layer
            (gimp-drawable-set-name 3d_bkg "3d_bkg")
            
            (if (= shapes 0)
                (set! shape 0)
            )
            (if (= shapes 1)
                (set! shape 1)
            )
            (if (= shapes 2)
                (set! shape 2)
            )
            (if (= shapes 3)
                (set! shape 3)
            )
            (if (= shapes 4)
                (set! shape 4)
            )
            
            ;(gimp-message "rendering random 3d shapes")
            ;(gimp-message (number->string shape)) 
            ;(gimp-message "density=")
            ;(gimp-message (number->string density)) 
            ;(gimp-message (number->string size))
            
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            
            
                ;; Render random3d using G'MIC.
                (plug-in-gmic-qt 1 image 3d_bkg 1 0 ; was 1 0 
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_random3d "
                        ;"0,80,3,100,45,0,0,-100,0.5,0.7,3,1"
                        ;"0,86.0,5.0,100.5,45.2,0.01,0.01,-93.8,0.5,0.7,3,0.98,"
                                 (number->string shape) ","
                    ;             (number->string density) ","
                                 "80,"
                                 (number->string size) ",100,45,0,0,-100,0.5,0.7,3,1"    ;;  was .01,-99.8,0.5,0.7,3,0.98
                    )
                )
            
                ;(gimp-displays-flush)
                ;(gimp-display-new image)
                ;(gimp-message "back from GMIC random3d")
                ;(quit)
                
                
                ;; Render smoothing using G'MIC.
                (plug-in-gmic-qt 1 image 3d_bkg 1 0 ; was 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_smooth_anisotropic 60,0.7,0.3,0.6,1.1,0.8,30,2,0,1,1,0,0,50,50" ; Was 60,0.9,0.88,0.7,3.5,1.01,30,2,0,1,1,1
                    )
                )
                
                
                
                ;;(gimp-hue-saturation 3d_bkg ALL-HUES 0 0 -100)
                ;; Make layers for animation
                ;(gimp-message "about to make seamless")
                (plug-in-make-seamless RUN-NONINTERACTIVE image 3d_bkg)
                ;(gimp-message "seamless done - off to move the layers")
                
                ;(gimp-displays-flush)
                ;(gimp-display-new image)
                
                
                (3d-fun-move-layer-anim image 3d_bkg frames direction FALSE TRUE 0 0 0 0 0 FALSE 0 100 0)
                
                
                
                ;(gimp-display-new image)
                ;(gimp-displays-flush)
                ;(quit)
                
                (gimp-image-remove-layer image bkg-layer)
                
        
        
        ; *******************************************end 3D_bkg
        ;       
        ;        
        ; *******************************************start 3D_fgd
        ;
        
        
            (set! 3D_fgd (car (gimp-layer-copy text-layer TRUE)))
            (set! txt-width (car (gimp-drawable-width text-layer)))
            (set! txt-height (car (gimp-drawable-height text-layer)))
            (set! 3d-height 40) ; was 12
            
            
                
            ;(gimp-message "start of 3D fgd")
            (set! 3d-height 3d-fgd-height)
            ;; Add a layers
            (gimp-image-insert-layer image 3D_fgd 0 1)
            
            
                
            ;; name the layers
            (gimp-drawable-set-name 3D_fgd "3D_fgd")
            (gimp-image-remove-layer image text-layer)
            (gimp-image-lower-layer-to-bottom image 3D_fgd)
            
            
            
            
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            ;(gimp-message "extrude start")
            ;(gimp-message (number->string 3d-height))
            ;(gimp-message (number->string 3d-fgd-height))
            ;(gimp-message (number->string 3d-smoothness))
            ;(gimp-message (number->string txt-width))
            ;(gimp-message (number->string txt-height))
            ;(quit)
            
            ;; Render extrude3d using G'MIC. 
            (plug-in-gmic-qt 1 image 3D_fgd 1 0 ; was 1 0
                (string-append
                    "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_extrude3d "
                        (number->string 3d-fgd-height) ",512,"
                        (number->string 3d-smoothness) ","                              ;0.8,"
                        (number->string txt-width) ","
                        (number->string txt-height) ",0.78,38.0,0.11,0.11,45,36.1,0.0,-50.0,0.5,0.7,4,1,0,0" ; was 0.78,53.0,0.11,0.15,45,95,0,-80,0.5,0.7,4,1
                )
            )
            ;(gimp-drawable-colorize-hsl 3D_fgd 90 70 0 )
            (plug-in-colorify 1 image 3D_fgd main-text-color)
            ;(gimp-message "extrude done")
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            
            
            
            ;(gimp-message "deinterlace start")
            
            ;; Render deinterlace using G'MIC.
            (plug-in-gmic-qt 1 image 3D_fgd 1 0 ; was 1 0
                (string-append
                    "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-deinterlace 1"
                )
            )
            ;(gimp-message "deinterlace done")
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            
            
                
            (set! texture-layer (car (gimp-layer-new image txt-width txt-height RGBA-IMAGE "Texture" 100 LAYER-MODE-MULTIPLY)))
            (gimp-image-insert-layer image texture-layer 0 -1)
            (gimp-layer-resize-to-image-size texture-layer)
            
            ;(gimp-message "after add and resize")
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            ;(gimp-message "check the texture")
            
            (if (= texture 0)
                (begin
                    ;(gimp-message "texture is 0")
                    ; NONE - No action
                    
                    ;;;;(gimp-context-set-background main-text-color)    ;;test
                    ;;;;(gimp-drawable-fill texture-layer FILL-BACKGROUND)
                )
            )
            
            (if (= texture 1)
                (begin
                    ;(gimp-message "texture is 1")
                    (gimp-context-set-background txt-color)    ;;test
                    (gimp-drawable-fill texture-layer FILL-BACKGROUND)
                )
            )
            (if (= texture 2)
                (begin
                    ;(gimp-message "texture is 2")
                    (gimp-context-set-pattern txt-pattern)   ;;test
                    (gimp-drawable-fill texture-layer FILL-PATTERN)                   
                )
            )
            (if (= texture 3)
                (begin
                    ;(gimp-message "texture is 3")
                    (gimp-context-set-gradient txt-gradient)
                    (plug-in-gradmap RUN-NONINTERACTIVE image 3D_fgd)  ;; apply the gradient
                )
            )
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            ;(gimp-message "end texture")
            
            
            
            (gimp-image-merge-down image texture-layer  CLIP-TO-IMAGE)
            
           ; (gimp-message "off to anim overlay bkgrd")
           (3d-fun-sg-anim-overlay-background image)
           ; (gimp-message "back from anim overlay bkgrd")
            
            
            
            (set! temp-layer (car (gimp-layer-new image txt-width txt-height RGBA-IMAGE "temp" 100 LAYER-MODE-NORMAL)))
            (gimp-image-insert-layer image temp-layer 0 -1)
            (gimp-image-lower-layer-to-bottom image temp-layer)
            (gimp-image-raise-layer image temp-layer)
            
            ;(gimp-displays-flush)
            ;(gimp-display-new image)
            ;(gimp-message "temp-layer up down middle")
            ;(quit)
                
                
            (gimp-image-merge-down image temp-layer CLIP-TO-IMAGE)
            (set! activeLayer (car (gimp-image-get-active-layer image)))
            (gimp-image-remove-layer image activeLayer)
            ;(gimp-displays-flush)
            ;(quit) 
            
            
            (gimp-context-pop)
            
            
            
        ;; Flush display.
        (gimp-displays-flush)
        ;; End undo group.
        (gimp-image-undo-group-end image)
        ;(gimp-message "Good finish OK alpha to logo")
    )
)

(script-fu-register "script-fu-3d-fun"
    "3D FUN..."
    "Uses GMIC,  to create a 3D animation. Set main text color to white to fully use texturing. Requires a layer with transparency. \nfile:3D-FUN_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "May 2014"
    "*"
    SF-IMAGE        "Image"                     0
    SF-DRAWABLE     "Drawable"                  0
    SF-OPTION       "Background Type"           '("Black" "White" "Pattern" "Gradient")
    SF-PATTERN      "Pattern"                   "Pink Marble"
    SF-GRADIENT     "Background Gradient"       "Abstract 3"
    SF-OPTION       "Background Shapes"         '("Cube" "Cone" "Cylinder" "Sphere" "Torus")
    SF-ADJUSTMENT   "Density of Shapes"         '(80 1 200 1 10 0 1)
    SF-ADJUSTMENT   "Size of Shapes"            '(3 1 20 1 5 0 1)
    SF-ADJUSTMENT   "3D-fgd-height"             '(130 1 400 1 5 0 1)
    SF-ADJUSTMENT   "3D-smoothness"             '(0.2 0.05 1.5 .01 0.05 2 1)
    SF-ADJUSTMENT   "Frames for Animation"      '(10 2 200 2 10 0 1)
    SF-OPTION       "Bkg Direction Options"     '("Up" "Down" "Left" 
                                                    "Right"
                                                    "Up and Left"
                                                    "Up and Right"
                                                    "Down and Left"
                                                    "Down and Right")
    SF-OPTION       "Texture Type" '("None" "Extra Texture Color" "Pattern" "Gradient")
    SF-COLOR        "Texture Color"    '(153 153 153)
    SF-PATTERN      "Texture Pattern"  "Blue Web"
    SF-GRADIENT     "Texture Gradient" "Incandescent"
    SF-COLOR        "Main Text Color"    '(240 240 0)
)

(script-fu-menu-register "script-fu-3d-fun" "<Toolbox>/Script-Fu/Alpha-to-Logo")

(define (script-fu-3d-fun-logo 
                                text
                                justify
                                letter-spacing
                                line-spacing
                                font-in
                                main-text-color
                                size
                                bkg-type 
                                pattern
                                gradient   
                                shapes
                                s-density
                                s-size
                                3d-fgd-height
                                3d-smoothness
                                frames
                                direction
                                texture
                                txt-color
                                txt-pattern
                                txt-gradient)
    (let* (
            (image-width 250)
            (image-height 250)
            (width image-width)
            (height image-height)
            (offx 0)
            (offy 0)
            (image (car (gimp-image-new image-width image-height RGB)))
            (border (/ size 4))
            (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
            (text-layer)
            (text-width 1)
            (text-height 1)
            (justify (cond  ((= justify 0) 2)
                            ((= justify 1) 0)
                            ((= justify 2) 1)))
            
          )
        (gimp-context-push)
        ;;;;adjust the text-layer
        (gimp-context-set-foreground main-text-color)
        (set! text-layer (car (gimp-text-fontname image -1 0 0 text border TRUE size PIXELS font)))
        (set! text-width (car (gimp-drawable-width text-layer)))
        (set! text-height (car (gimp-drawable-height text-layer)))
            
        (gimp-text-layer-set-justification text-layer justify)
        (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
        (gimp-text-layer-set-line-spacing text-layer line-spacing)
        (set! text-width (car (gimp-drawable-width text-layer)))
        (set! text-height (car (gimp-drawable-height text-layer)))
        
        ;;;;set the new Image size
        (if (> text-width image-width) (set! width text-width))           
        (if (> text-height image-height) (set! height text-height))	
        
        
        
        ;;;;resize the image	
        (gimp-image-resize image width height 0 0)
        
        
        
        (set! offx (/ (- width text-width) 2))
        (set! offy (/ (- height text-height) 2))    
        (gimp-image-resize image width height 0 0)
        (gimp-layer-set-offsets text-layer offx offy)
        
        
        
        (gimp-image-set-active-layer image text-layer);;test-OK
        (gimp-layer-resize-to-image-size text-layer)
        
        ;(gimp-message (number->string text-width))
        ;(gimp-message (number->string text-height))
        ;(gimp-message (number->string width))
        ;(gimp-message (number->string height))
        
        
        (script-fu-3d-fun image text-layer
                            bkg-type 
                            pattern
                            gradient 
                            shapes
                            s-density
                            s-size
                            3d-fgd-height
                            3d-smoothness
                            frames
                            direction
                            texture
                            txt-color
                            txt-pattern
                            txt-gradient
                            main-text-color)
        
        
        (gimp-context-pop)
        
        (gimp-display-new image)
        (gimp-displays-flush)
        ;(gimp-message "Good finish OK")
    )
)

(script-fu-register "script-fu-3d-fun-logo"
    "3D-FUN Logo"
    "Uses GMIC, to create a 3D animation. Set main text color to white to fully use texturing.  \nfile:3D-FUN_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "May 2014"
    ""
    SF-TEXT         "Text"              "Gimp 3D fun"
    SF-OPTION       "Justify"           '("Centered" "Left" "Right") 
    SF-ADJUSTMENT   "Letter Spacing"    '(50 -100 100 1 5 0 0)
    SF-ADJUSTMENT   "Line Spacing"      '(0 -100 100 1 5 0 0)
    SF-FONT         "Font"              "Sans-serif Bold" 
    SF-COLOR        "Main Font color"   '(245 245 0)
    SF-ADJUSTMENT   "Font size (pixels)"    '(300 6 500 1 1 0 1)
    SF-OPTION       "Background Type"       '("Black" "White" "Pattern" "Gradient")
    SF-PATTERN      "Pattern"               "Pink Marble"
    SF-GRADIENT     "Background Gradient"   "Abstract 3"  
    SF-OPTION       "Background Shapes"     '("Cube" "Cone" "Cylinder" "Sphere" "Torus")
    SF-ADJUSTMENT   "Density of Shapes"     '(80 1 200 1 10 0 1)
    SF-ADJUSTMENT   "Size of Shapes"        '(5 1 20 1 5 0 1)
    SF-ADJUSTMENT   "3D-fgd-height"         '(130 1 400 1 5 0 1)
    SF-ADJUSTMENT   "3D-smoothness"         '(0.20 0.05 1.51 .01 0.10 2 0)
    SF-ADJUSTMENT   "Frames for Animation"  '(10 2 200 2 10 0 1)
    SF-OPTION       "Bkg Direction Options" '("Up" "Down" "Left" "Right" "Up and Left" "Up and Right" "Down and Left" "Down and Right")
    SF-OPTION       "Texture Type"          '("None" "Extra Color" "Pattern" "Gradient")
    SF-COLOR        "Texture Color"         '(153 153 153)
    SF-PATTERN      "Texture Pattern"       "Blue Web"
    SF-GRADIENT     "Texture Gradient"      "Incandescent"
)

(script-fu-menu-register "script-fu-3d-fun-logo" "<Image>/Script-Fu/Logos")
  
;;--------------------------------------------------------------------------

(define (3d-fun-move-layer-anim image   
                layer
                frames
                direction
                pingPong
                pingCopy
                layerHandle
                startX
                startY
                endX
                endY
                addPath
                blendMode
                opacity
                fillOption
        )
        
        ; Declare the Variables
        
    (let* (
            (height 0)
            (width 0)
            (copyLayer 0)
            (origSelect 0)
            (wrap 0)
            (x-dir
                (cond
                    (( equal? direction 0 ) 0)
                    (( equal? direction 1 ) 0)
                    (( equal? direction 2 ) -1)
                    (( equal? direction 3 ) 1)
                    (( equal? direction 4 ) -1)
                    (( equal? direction 5 ) 1)
                    (( equal? direction 6 ) -1)
                    (( equal? direction 7 ) 1)
                )
            )       
            (y-dir
                (cond
                    (( equal? direction 0 ) -1)  
                    (( equal? direction 1 ) 1)
                    (( equal? direction 2 ) 0)
                    (( equal? direction 3 ) 0)		
                    (( equal? direction 4 ) -1)		
                    (( equal? direction 5 ) -1)		
                    (( equal? direction 6 ) 1)		
                    (( equal? direction 7 ) 1)
                )
            )       
            (step-x 0)
            (step-y 0)
            (offset-x 0)
            (offset-y 0)
            (mode-lut '#(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14))
            (vectors 0)
            (pathLength 0)
            (dist 0)
            (distInc 0)
            (points (cons-array 12 'double))
            (numLayers 0)
            (layerID 0)
            (stackPos 1)
            (fillColor 0)
          )
        
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        ;(gimp-message "inside move layer anim")
        
        (set! origSelect (car (gimp-selection-save image)))
        (gimp-selection-none image) 
        
        (set! width (car (gimp-drawable-width layer)))
        (set! height (car (gimp-drawable-height layer)))
        
        (gimp-layer-add-alpha layer)
        (gimp-layer-set-mode layer (vector-ref mode-lut blendMode))
        (gimp-layer-set-opacity layer opacity)
        
        (if (= pingPong TRUE)
            (set! frames (ceiling (/ frames 2)))
        )
        
        ;(gimp-message "is direction 9")
        (if (= direction 9)
            (begin
                (set! startX (car (gimp-drawable-offsets layer)))
                (set! startY (cadr (gimp-drawable-offsets layer)))
            )
        )
        
        (if (= direction 10)
            (set! vectors (car (gimp-image-get-active-vectors image)))
        )
        
        
        (if (> direction 7)
            (begin
                ;(gimp-message "direction is gr than 7")
                
                (if (or (= direction 8)(= direction 9))
                    (begin
                        ;(gimp-message "dir is 8 or 9")
                        (set! vectors (car (gimp-vectors-new image "New Path")))
                        (aset points 0 startX)	
                        (aset points 1 startY)
                        (aset points 2 startX)
                        (aset points 3 startY)
                        (aset points 4 startX)
                        (aset points 5 startY)
                        (aset points 6 endX)
                        (aset points 7 endY)
                        (aset points 8 endX)
                        (aset points 9 endY)
                        (aset points 10 endX)
                        (aset points 11 endY)	
                        (gimp-vectors-stroke-new-from-points vectors 0 12 points FALSE)
                    )
                )
                
                
                
                
                (set! pathLength (trunc (car (gimp-vectors-stroke-get-length vectors 1 1))))
                (set! distInc (/ pathLength (- frames 1)))
                
                (if (and (or (= direction 8)(= direction 9)) (= addPath TRUE))
                    (gimp-image-add-vectors image vectors -1)
                )
                
                (while (>= frames 0)
                    
                    (set! offset-x (car (gimp-vectors-stroke-get-point-at-dist vectors 1 dist 1)))
                    (set! offset-y (cadr (gimp-vectors-stroke-get-point-at-dist vectors 1 dist 1)))
                    (set! copyLayer (car (gimp-layer-copy layer TRUE)))
                    
                    (if (> frames 0)
                        (gimp-image-insert-layer image copyLayer 0 -1)
                    )   
                    
                    
                    (cond
                        ((= layerHandle 1) ; top middle
                            (set! offset-x (- offset-x (/ width 2)))
                        )
                        ((= layerHandle 2) ; top right
                            (set! offset-x (- offset-x width))
                        )
                        ((= layerHandle 3) ; middle left
                            (set! offset-y (- offset-y (/ height 2)))
                        )
                        ((= layerHandle 4) ; center
                            (set! offset-x (- offset-x (/ width 2)))
                            (set! offset-y (- offset-y (/ height 2)))
                        )
                        ((= layerHandle 5) ; middle right
                            (set! offset-x (- offset-x width))
                            (set! offset-y (- offset-y (/ height 2)))
                        )
                        ((= layerHandle 6) ; bottom left
                            (set! offset-y (- offset-y height))
                        )
                        ((= layerHandle 7) ; bottom middle
                            (set! offset-x (- offset-x (/ width 2)))
                            (set! offset-y (- offset-y height))
                        )
                        ((= layerHandle 8) ; bottom right
                            (set! offset-x (- offset-x width))
                            (set! offset-y (- offset-y height))
                        )
                        
                    )
                    
                    (gimp-layer-set-offsets copyLayer offset-x offset-y)
                    (if (> frames 0)
                        (gimp-layer-resize-to-image-size copyLayer)
                    )
                    (set! frames (- frames 1))
                    (set! dist (+ dist distInc))
                ); while closure
                
                ;(gimp-image-remove-layer image layer)
                
            ); begin closure
            
            ; Else direction is <= 7
            (begin
                ;(gimp-message "direction is less than 7 or equal")
                (set! step-x (* x-dir (/ width frames)))
                (set! step-y (* y-dir (/ height frames)))
                (set! offset-x step-x)
                (set! offset-y step-y)
                (set! frames (- frames 1))
                
                (while (>= frames 1)
                    (set! copyLayer (car (gimp-layer-copy layer TRUE)))
                    (gimp-image-add-layer image copyLayer -1)
                    
                    (if (= fillOption 0)
                        (begin
                            (set! wrap TRUE)
                            (set! fillColor 1)
                        )
                    )
                    
                    (if (= fillOption 1)
                        (begin
                            (set! wrap FALSE)
                            (set! fillColor 0)
                        )
                    )
                    
                    (if (= fillOption 2)
                        (begin
                            (set! wrap FALSE)
                            (set! fillColor 1)
                        )
                    )
                    
                    (gimp-drawable-offset copyLayer wrap fillColor offset-x offset-y)
                    
                    (set! offset-x (+ offset-x step-x))
                    (set! offset-y (+ offset-y step-y))
                    
                    (set! frames (- frames 1))
                ); while closure
                
            ); begin closure direction < 7
            
        ); if direction > 7 closure 
        
        ;(gimp-message "check for PingPong")
        (if (= pingPong TRUE)
            (begin
                ;(gimp-message "inside pingpong")
                (if (= pingCopy TRUE)
                    (set! numLayers (- (car (gimp-image-get-layers image)) 1))
                    (set! numLayers (car (gimp-image-get-layers image)))
                )
                
                (set! layerID (cadr (gimp-image-get-layers image)))
                
                (while (> numLayers 2)
                    (set! layer (car (gimp-image-set-active-layer image (aref layerID stackPos))))
                    (set! layer (car (gimp-image-get-active-layer image)))
                    (set! copyLayer (car (gimp-layer-copy layer TRUE)))
                    (gimp-image-insert-layer image copyLayer 0 0)
                    (set! numLayers (- numLayers 1))
                    (set! stackPos (+ stackPos 1))
                )
            )
        )
        
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
        
        (gimp-image-crop image width height 0 0)
        
        (gimp-selection-load origSelect)
        (gimp-image-remove-channel image origSelect)    
        
        ;(gimp-displays-flush)   
        (gimp-image-undo-group-end image) 
        (gimp-context-pop)
        ;(gimp-message "leaving anim layer")
        
        
    )
)

(define (3d-fun-sg-anim-overlay-background image)
    
    ; get visible layers (bottom-to-top)
    (define (get-visibles image)
        (let loop ((layers (vector->list (cadr (gimp-image-get-layers image))))
                    (visibles '())
                  )
            (if (null? layers)
                (begin
                  visibles
                )
                (begin
                 (loop (cdr layers) 
                    (if (zero? (car (gimp-drawable-get-visible (car layers))))
                        visibles
                        (cons (car layers) visibles) ) )
                )
            
            ) 
        ) 
    )
    
    (gimp-image-undo-group-start image)
    (let* (
            (layers (vector->list (cadr (gimp-image-get-layers image))))
            (visibles (get-visibles image))
            (bg-layer (car (last layers)))
            (orig-sel (car (gimp-selection-save image)))
            (layer)
            (position 0) 
            (over-layer 0)
          )
        (gimp-selection-none image)
        (map (lambda (x) (gimp-drawable-set-visible x FALSE)) visibles)
        (when (= (car visibles) bg-layer)
            (set! visibles (cdr visibles))
            (gimp-drawable-set-visible bg-layer TRUE)
        )
        (while (pair? visibles)
            
                (set! layer (car visibles))
                (set! position (car (gimp-image-get-layer-position image layer)))
                  
                (gimp-drawable-set-visible layer TRUE)
                (gimp-image-set-active-layer image layer)
                (set! over-layer (car (gimp-layer-new-from-drawable bg-layer image)))
                (gimp-image-insert-layer image over-layer 0 -1)
                (gimp-drawable-set-visible over-layer TRUE)
                (gimp-image-merge-down image over-layer EXPAND-AS-NECESSARY)
                
                (set! visibles (cdr visibles))
        )
        (gimp-selection-load orig-sel)
        (gimp-image-remove-channel image orig-sel)
    )
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
)

;end of script 