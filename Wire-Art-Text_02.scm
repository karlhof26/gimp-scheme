; Wire Text rel 0.05
; Created by Graechan
; 
;    Created from this video tutorial at http://www.youtube.com/watch?v=N-3YuZlZn7Q&feature=related
;    that inspired me to write this script. 
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
; Rel 0.02 - Added multi-line text with justification and spacing
;            Changed ellipse to circular bkg to fit multi-line text
;            Added choice to add bkg to image only
; Rel 0.03 - Replaced the ellipse shape
; Rel 0.04 - Added Alpha-to-Logo script, Shapes now consist of ("Rectangular" "Square" "Ellipse" "Circular"), new setting to adj Frame-size, new option to retain the original layer
; Rel 0.05 - Added Bkg choices-Color, Pattern, or Gradient. Also Added Bkg crop options of None, Crop Inner or Crop Outer
;
; Gradients blend direction list
(define list-blend-dir '("Left to Right" "Top to Bottom" "Diagonal" "Diagonal to centre" "Diagonal from centre"))
;

(define (script-fu-wire-art-text text
                                justify
                                letter-spacing
                                line-spacing								 
                                font-in 
                                font-size
                                grow
                                original-layer
                                shape
                                size-adj
                                wire-size
                                metal
                                tint-color
                                metal-text
                                bkg-type
                                bkg-color
                                bkg-pattern
                                bkg-gradient
                                gradient-type
                                reverse
                                blendir
                                bkg-crop)
                                
  (let* (
         (img-width 250)
         (img-height 250)
         (width img-width)
         (height img-height)
         (offx 0)
         (offy 0)
         (image (car (gimp-image-new img-width img-height RGB)))
         (border (/ font-size 3))
         (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
         (text-layer (car (gimp-text-fontname image -1 0 0 text border TRUE font-size PIXELS font)))
         (text-width (car (gimp-drawable-width text-layer)))
         (text-height (car (gimp-drawable-height text-layer)))
         (bkg-layer 0)
         (selection-channel 0)
         (img-channel 0)
         (tint-layer 0)        
        )
        
    (gimp-context-push)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))    
    
    ;;;;position the text lines
    (set! justify (cond ((= justify 0) 2)
                        ((= justify 1) 0)
                        ((= justify 2) 1)))
    (gimp-text-layer-set-justification text-layer justify)
    (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
    (gimp-text-layer-set-line-spacing text-layer line-spacing)
    
    (set! text-width (car (gimp-drawable-width text-layer)))
    (set! text-height (car (gimp-drawable-height text-layer)))
    
    ;;;;set the new Image size
    ;	(if (> text-width img-width) (set! width text-width))           
    ;    (if (> text-height img-height) (set! height text-height))
    (gimp-image-resize-to-layers image)
    
    
    ;;;;Expand the font if needed
    (if (> grow 0)
           (begin
            (gimp-selection-layer-alpha text-layer)
            (gimp-edit-clear text-layer)
            (gimp-selection-grow image grow)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill text-layer FILL-FOREGROUND)
            (gimp-selection-none image)
           )
    )
    (gimp-selection-none image) 
    
    ;(gimp-display-new image)
    
    (gimp-image-undo-group-start image)
    (script-fu-wire-art-alpha image 
                                text-layer
                                original-layer
                                shape
                                size-adj
                                wire-size
                                metal
                                tint-color
                                metal-text
                                bkg-type
                                bkg-color
                                bkg-pattern
                                bkg-gradient
                                gradient-type
                                reverse
                                blendir
                                bkg-crop)
    (gimp-image-undo-group-end image)
    
    (gimp-context-pop)      
    (gimp-display-new image)
    
    
    
    )
  )
  
(script-fu-register "script-fu-wire-art-text"
    "Wire Art Text"
    "Creates Wire Shaped from Text. \nfile:Wire0Art-Test_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Jan 2015"
    ""
    SF-TEXT       "Text"    "Wire\nArt\nText" 
    SF-OPTION "Justify" '("Centered" "Left" "Right")
    SF-ADJUSTMENT "Letter Spacing" '(0 -100 100 1 5 0 0)
    SF-ADJUSTMENT "Line Spacing" '(0 -100 100 1 5 0 0)
    SF-FONT       "Font"               "Sans-serif Bold"
    SF-ADJUSTMENT "Font size (pixels)" '(180 100 300 1 1 0 0)
    SF-ADJUSTMENT "Expand the Font if needed" '(3 0 6 1 1 0 1)
    SF-TOGGLE     "Keep the Original layer"   FALSE
    SF-OPTION "Image Shape" '("Rectangular" "Square" "Ellipse" "Circular")
    SF-ADJUSTMENT "Image Size Adj. (pixels)" '(0 0 100 1 10 0 0)
    SF-ADJUSTMENT "Wire Size (pixels)" '(8 2 20 1 1 0 0)
    SF-OPTION       "Metal Plating Type" '("Chrome" "Gold" "Silver" "Copper" "Bronze" "Colored Tint")
    SF-COLOR        "Tint Color"         '(0 0 255)
    SF-TOGGLE       "Plate the Text only"   FALSE
    SF-OPTION       "Background Type" '("Color" "Pattern" "Gradient" "Transparency")
    SF-COLOR        "Background color"         "lightgrey"
    SF-PATTERN      "Background Pattern"            "Bricks"
    SF-GRADIENT     "Background Gradient" "Abstract 3"
    SF-ENUM         "Gradient Fill Mode" '("GradientType" "gradient-linear")
    SF-TOGGLE       "Reverse the Gradient"   FALSE
    SF-OPTION       "Blend Direction" 		list-blend-dir
    SF-OPTION "Background Crop Types" '("None" "Crop Inner" "Crop Outer") 
  )
(script-fu-menu-register "script-fu-wire-art-text"
  "<Image>/Script-Fu/Logos/" 
  )

(define (script-fu-wire-art-alpha image 
                                drawable
                                original-layer
                                shape
                                size-adj
                                wire-size
                                metal
                                tint-color
                                metal-text
                                bkg-type
                                bkg-color
                                bkg-pattern
                                bkg-gradient
                                gradient-type
                                reverse
                                blendir
                                bkg-crop)
                                
    (gimp-image-undo-group-start image)
    
 (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        (offx 0)
        (offy 0)
        (new-width 0)
        (new-height 0)
        (copy 0)
        (bkg-layer 0)
        (selection-channel 0)
        (img-channel 0)
        (tint-layer 0)
        (x1 0)
        (y1 0)
        (x2 0)
        (y2 0)
        
        (grid-color)
        
       )	
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    
    
    ;;;;square up the Image if needed
    (if (or(= shape 1) (= shape 3)) 
        (begin
            (if (< height width) (set! height width))
            (if (< width height) (set! width height)))
    )
    
    ;;;;resize the image	
    (gimp-image-resize image width height 0 0)   
    ;;;;centre the text layer	
    (set! offx (/ (- width (car (gimp-drawable-width drawable))) 2))
    (set! offy (/ (- height (car (gimp-drawable-height drawable))) 2))    
    (gimp-layer-set-offsets drawable offx offy)
    (gimp-layer-resize-to-image-size drawable)	
    
    ;(gimp-displays-flush)
    ;(gimp-display-new image)
    
    (cond ((> size-adj 0)
        (set! new-width (+ width (* (/ width 100) size-adj)))
        (set! new-height (+ height (* (/ height 100) size-adj)))
        (gimp-image-resize image new-width new-height (/(- new-width  width) 2) (/(- new-height  height) 2))
        (gimp-layer-resize-to-image-size drawable)
        (set! width new-width)
        (set! height new-height)
        )
    )
    
    (gimp-progress-update 0.10)
    (set! copy (car (gimp-layer-copy drawable TRUE)))
    (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer image bkg-layer 0 1)
    
        (if (> shape 1)
            (gimp-ellipse-select image 0 0 (- width 5) (- height 5) 2 TRUE FALSE 0)
        )
        (if (< shape 2) 
            (begin
                (gimp-selection-all image)
                (gimp-selection-shrink image 10)
                (script-fu-selection-rounded-rectangle image bkg-layer 50 FALSE)
            )
        )
        ;;;;create channel
        (gimp-selection-save image)
        (set! selection-channel (car (gimp-image-get-active-drawable image)))	
        (gimp-channel-set-opacity selection-channel 100)	
        (gimp-drawable-set-name selection-channel "Shape Selection") 
        (gimp-image-set-active-layer image bkg-layer)
        
        (gimp-edit-fill bkg-layer FILL-FOREGROUND)
        (gimp-selection-shrink image wire-size)
        (gimp-edit-clear bkg-layer)
        
        (cond
                ((= metal 0)
                    (set! grid-color '(10 10 10))
                )
                ((= metal 1)
                    (set! grid-color '(248 215 95)) ; '(255 215 0)
                )
                ((= metal 2)
                    (set! grid-color '(192 191 192))
                )
                ((= metal 3)
                    (set! grid-color '(250 180 150))
                )
                ((= metal 4)
                    (set! grid-color '(166 125 61))
                )
                ((= metal 5)
                    (set! grid-color tint-color)
                )
        )
        
        (plug-in-grid 1 image bkg-layer 2 16 8 '(86 86 86) 255 2 16 8 '(86 86 86) 255 0 2 6 '(86 86 86) 255) ; was black
        (gimp-selection-layer-alpha drawable)
        ;;;;create channel
        (gimp-selection-save image)
        (set! img-channel (car (gimp-image-get-active-drawable image)))	
        (gimp-channel-set-opacity img-channel 100)
        (gimp-drawable-set-name img-channel "img-channel")
        (gimp-image-set-active-layer image drawable)
        (gimp-selection-shrink image wire-size)
        
        ;(gimp-displays-flush)
        (gimp-progress-update 0.20)
        ;(gimp-message "quitting")
        ;(quit)
        
        ;(set! drawable (car (gimp-image-merge-down image drawable EXPAND-AS-NECESSARY)))
        (gimp-drawable-set-name drawable "Wire Text")
        (gimp-edit-clear drawable)
        (gimp-selection-none image)
        (gimp-progress-update 0.30)
        ;(script-fu-chrome-logo-alpha image drawable 10 bkg-color)
        
        
        ;(plug-in-colorify 1 image drawable '(230 230 0))
        (gimp-drawable-colorize-hsl drawable 293.2 80.1 88.2)
        ;(gimp-displays-flush)
        
        
        (FU-chrome-image image drawable 10 5 5 TRUE TRUE TRUE)
        
        (if (= metal 1) (gimp-context-set-foreground '(255 215 0)))   ;gold 
        (if (= metal 2) (gimp-context-set-foreground '(192 192 192))) ;silver
        (if (= metal 3) (gimp-context-set-foreground '(230 102 34))) ;copper '(250 180 150)))
        (if (= metal 4) (gimp-context-set-foreground '(166 125 61)))  ;bronze
        (if (= metal 5) (gimp-context-set-foreground tint-color))     ;colored tint
        
        (if (> metal 0)
            (begin
                (gimp-progress-update 0.35)
                (set! tint-layer (car (gimp-layer-new image width height RGBA-IMAGE "Tint" 100 LAYER-MODE-MULTIPLY)))
                (gimp-image-insert-layer image tint-layer 0 -1)
                (if (= metal-text TRUE)
                    (gimp-selection-load img-channel)
                )
                (gimp-edit-fill tint-layer FILL-FOREGROUND)
                (set! drawable (car (gimp-image-merge-down image tint-layer CLIP-TO-IMAGE)))
                (if (< metal 5)
                    (gimp-brightness-contrast drawable 50 50)
                )
                (gimp-selection-none image)
            )
        )
        (if (= metal 0)
            (begin 
                (set! tint-layer (car (gimp-layer-new image width height RGBA-IMAGE "Tint" 100 LAYER-MODE-MULTIPLY)))
                (gimp-image-insert-layer image tint-layer 0 -1) 
                (define drawable (car (gimp-image-merge-down image tint-layer CLIP-TO-IMAGE)))
            )
        )
        (gimp-progress-update 0.7)
        (set! drawable (car (gimp-image-merge-down image drawable EXPAND-AS-NECESSARY)))
        (gimp-drawable-set-name drawable "Wire Text")
        (gimp-progress-update 0.75)
        (plug-in-autocrop 1 image drawable)
    
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Tint" 100 LAYER-MODE-MULTIPLY)))
        (gimp-image-insert-layer image bkg-layer 0 1) 
        (gimp-progress-update 0.77)
        ;(set! bkg-layer (car (gimp-image-merge-down image bkg-layer CLIP-TO-IMAGE)))
        ;(gimp-displays-flush)
        
        (if (= bkg-type 3)
            (gimp-edit-clear bkg-layer)
        )
        (gimp-progress-update 0.80)
        (gimp-context-set-background bkg-color)
        (gimp-context-set-pattern bkg-pattern)
        (gimp-context-set-gradient bkg-gradient)  
            
        (if (= bkg-type 0)
            (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
        )
        (if (= bkg-type 1) (gimp-drawable-fill bkg-layer FILL-PATTERN))
        (if (= bkg-type 2)
            (begin
                (if (= blendir 0) (set! x2 width)) 
                (if (= blendir 1) (set! y2 height))
                (if (= blendir 2)
                    (begin
                        (set! x2 width)
                        (set! y2 height)
                    )
                )
                (if (= blendir 3)
                    (begin
                        (set! x2 (* width 0.7)) ;  was 1/ 2
                        (set! y2 (* height 0.7)) ;was height/2
                    )
                )
                (if (= blendir 4)
                    (begin
                        (set! x1 (/ width 2))
                        (set! y1 (/ height 2))
                    )
                )
                (if (and (>= gradient-type 6) (<= gradient-type 8))
                    (begin
                        (set! x1 (* width 0.1))
                        (set! y1 (* width 0.1))
                        (set! x2 (* width 0.2))
                        (set! y2 (* height 0.2))
                        (gimp-selection-load img-channel)
                        (gimp-selection-invert image)
                        ;(gimp-image-select-rectangle image CHANNEL-OP-ADD (* width 0.02) (* height 0.02) (* width 0.98) (* height 0.98) )
                    )
                )
                (gimp-edit-blend bkg-layer BLEND-CUSTOM LAYER-MODE-NORMAL gradient-type 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE x1 y1 x2 y2)
            )
        ) ;endif
        
        (gimp-progress-update 0.82)
        
    (cond ((= original-layer TRUE)
            (gimp-image-insert-layer image copy 0 -1)
            (set! width (car (gimp-image-width image)))
            (set! height (car (gimp-image-height image)))
            (set! offx (/ (- width (car (gimp-drawable-width copy))) 2))
            (set! offy (/ (- height (car (gimp-drawable-height copy))) 2))    
            (gimp-layer-set-offsets copy offx offy)
            (gimp-layer-resize-to-image-size copy)
        )
    )
    
    
    (cond ((> bkg-crop 0)
        (gimp-selection-load selection-channel)
        (if (= bkg-crop 2) (gimp-selection-invert image))
        (gimp-edit-clear bkg-layer)
        (gimp-selection-none image)
        )
    )
    
    
    (gimp-context-pop)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    ;(gimp-message "line398")
    
 )    
)

(script-fu-register "script-fu-wire-art-alpha"        		    
    "Wire Art"
    "Creates Wire Shaped from Alpha Channel. \nfile:Wire-Art-Test_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Jan 2015"
    "RGB*"
    SF-IMAGE        "image"      0
    SF-DRAWABLE     "drawable"   0
    SF-TOGGLE       "Keep the Original layer"   FALSE
    SF-OPTION       "Image Shape" '("Rectangular" "Square" "Ellipse" "Circular")
    SF-ADJUSTMENT   "Image Size Adj. (pixels)"      '(0 0 100 1 10 0 0)
    SF-ADJUSTMENT   "Wire Size (pixels)"            '(8 2 20 1 1 0 0)
    SF-OPTION   "Metal Plating Type"    '("Chrome" "Gold" "Silver" "Copper" "Bronze" "Colored Tint")
    SF-COLOR    "Tint Color"            '(0 0 255)
    SF-TOGGLE   "Plate the Text only"   FALSE
    SF-OPTION   "Background Type"       '("Color" "Pattern" "Gradient" "Transparency")
    SF-COLOR    "Background Color"                  '(211 211 211)
    SF-PATTERN  "Background Pattern"                "Bricks"
    SF-GRADIENT "Background Gradient"               "Abstract 3"
    SF-ENUM     "Gradient Fill Mode"                '("GradientType" "gradient-linear")
    SF-TOGGLE   "Reverse the Gradient"              FALSE
    SF-OPTION   "Blend Direction"                   list-blend-dir
    SF-OPTION   "Background Crop Types"             '("None" "Crop Inner" "Crop Outer")
  
)

(script-fu-menu-register "script-fu-wire-art-alpha" "<Image>/Script-Fu/Alpha-to-Logo/")

;end of script