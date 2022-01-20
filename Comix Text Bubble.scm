; Comix Text Bubble rel 0.05 
; Created by Graechan
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
; Rel 0.01 - Initial Release
; Rel 0.02 - Added an additional width and height setting to accomodate long text strings
; Rel 0.03 - Changed the script to function on a active drawable and moved to Filters>Decor
; Rel 0.04 - Added extra pointer orientation options and no pointer to pointer type also a blank text box allowed for pictures 
; (just set add-width and add-height to required size)
; Rel 0.05 - Added shape hugging choice to bubble shapes, also added control for outline size 
(define (script-fu-comix-text-bubble image drawable
                                text-in 
                                color 
                                font-in 
                                font-size
                                add-width
                                add-height
                                shape
                                type
                                orientation
                                outline
                                outline-color
                                bubble-fill
                                bubble-color
                                shadow
                                conserve
                              )
                                
        
 (let* (
            (image-layer (car (gimp-image-get-active-layer image)))
            (offx 0)
            (offy 0)
            (text (if (> (string-length text-in) 0) text-in "Img"))
            (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
            (width (+ (car (gimp-text-get-extents-fontname text font-size PIXELS font)) (+ (/ font-size 2) add-width)))
            (height (+ (cadr (gimp-text-get-extents-fontname text font-size PIXELS font)) (+ (/ font-size 2) add-height)))
            (border (/ font-size 2))
            (text-layer 0)
            (text-width 0)
            (text-height 0)
            (bkg-layer 0)
            (x 0)
            
        )   
        
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        (gimp-context-set-default-colors)
        (gimp-context-set-antialias TRUE)
        
        (set! text-layer (car (gimp-text-fontname image -1 0 0 text border TRUE font-size PIXELS font)))
        (set! text-width (car (gimp-drawable-width text-layer)))
        (set! text-height (car (gimp-drawable-height text-layer)))
        (gimp-drawable-set-name text-layer "Comix Text")
        
        ;;;;centre text on line
        (gimp-text-layer-set-justification text-layer 2)
        ;;;;set the new layer size
        (if (> text-width width)
            (set! width text-width)
        )           
        (if (> text-height height) (set! height text-height))	
        
        ;;;;resize the image	
        ;(gimp-layer-resize text-layer width height 0 0)
        (set! bkg-layer (car (gimp-layer-new image width (+ height (/ height 1.85)) RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL-LEGACY)))
        (gimp-image-insert-layer image bkg-layer 0 -1)
        (gimp-image-lower-layer image bkg-layer)
        
        ;;;;centre the text layer	
        (set! offx (/ (- width text-width) 2))
        (set! offy (/ (- height text-height) 2))    
        (gimp-layer-set-offsets text-layer offx offy)
        ;(gimp-layer-resize-to-image-size text-layer)
        (set! text-width (car (gimp-drawable-width text-layer)))
        (set! text-height (car (gimp-drawable-height text-layer)))
        
        ;;;;set the text clolor    
        (gimp-context-set-foreground color)
        (gimp-selection-layer-alpha text-layer)
        (gimp-edit-fill text-layer FILL-FOREGROUND)
        (gimp-selection-none image)
        
        ;;;;begin the script
        (set! width (car (gimp-drawable-width bkg-layer)))
        (set! height (car (gimp-drawable-height bkg-layer)))
        
        ;;;;create the speech bubble
        (if (= type 0)
            (begin
                (gimp-context-set-antialias TRUE)
                (set! x (- (* width 0.5) (* width 0.2)))
                ; (gimp-ellipse-select image x (* height 0.5) (* width 0.4) (* height 0.4) 2 TRUE FALSE 0)
                (gimp-image-select-ellipse image CHANNEL-OP-REPLACE x (* height 0.5) (* width 0.4) (* height 0.4))
                (set! x (- (* width 0.5) (* width 0.2) (* width 1 -0.1)))
                ;(gimp-ellipse-select image x (* height 0.5) (* width 0.4) (* height 0.4) 1 TRUE FALSE 0)
                (gimp-image-select-ellipse image CHANNEL-OP-SUBTRACT x (* height 0.5) (* width 0.4) (* height 0.4))
                (if (or (< orientation 2) (and (> orientation 2) (< orientation 5)))
                    (gimp-selection-translate image (* width 1 0.3) 0)
                )
            )
        )
        
        ;;;;create the thought bubble
        (if (= type 1)
            (begin
                (set! x (+ (* width 0.5) (* width -0.025) (* width 1 0.3)))
                ; (gimp-ellipse-select image x (* height 0.85) (* width 0.05) (* height 0.05) 2 TRUE FALSE 0)
                (gimp-image-select-ellipse-select image x CHANNEL-OP-REPLACE (* height 0.85) (* width 0.05) (* height 0.05))
                (set! x (+ (* width 0.5) (* width -0.05) (* (* width 1) 0.2)))
                ; (gimp-ellipse-select image x (* height 0.75) (* width 0.1) (* height 0.1) 0 TRUE FALSE 0)
                (gimp-ellipse-select image CHANNEL-OP-ADD x (* height 0.75) (* width 0.1) (* height 0.1))
                (if (or (= orientation 2) (= orientation 5))
                    (gimp-selection-translate image (* 1 width -0.3) 0)
                )
            )
        )   
        
        (if (< type 2)
            (gimp-edit-fill bkg-layer FILL-BACKGROUND)
        )
        
    ;;;;create the bubble shape
    (if (= shape 0)
        (gimp-ellipse-select image 0 0 width (+ text-height add-height) 0 TRUE FALSE 0)
    )
    (if (= shape 1)
        (begin
            ;(gimp-rect-select image 0 0 width text-height 0 FALSE 0)
            (gimp-image-select-rectangle image CHANNEL-OP-ADD 0 0 width text-height)
        )
    )
    (if (= shape 2)
        (gimp-image-select-round-rectangle image CHANNEL-OP-ADD 0 0 width text-height 50 50)
    )
    (if (< shape 3)
        (begin   
            ;;;;create the outline
            (gimp-context-set-foreground outline-color)
            (gimp-edit-fill bkg-layer FILL-FOREGROUND)
            
            ;;;;create the bubble fill	
            (if (= bubble-fill TRUE)
                (begin
                    (gimp-selection-shrink image outline)
                    (gimp-context-set-background bubble-color)
                    (gimp-edit-fill bkg-layer FILL-BACKGROUND)
                )
            )
        )
    )
    
    (gimp-selection-none image)
    
    (if (or (= orientation 0) (= orientation 3)) 
        (gimp-drawable-transform-flip-simple bkg-layer 0 TRUE 0 FALSE)
    )
    
    (if (> orientation 2)
        (begin
            (gimp-drawable-transform-flip-simple bkg-layer 1 FALSE (/ text-height 2) FALSE)
            (gimp-layer-set-offsets bkg-layer 0 0)
            (gimp-layer-set-offsets text-layer (car (gimp-drawable-offsets text-layer)) (- height (+ text-height (cadr (gimp-drawable-offsets text-layer)))))
        )
    )
    
    (if (= shape 3)
        (begin
            (gimp-selection-layer-alpha text-layer)
            (gimp-selection-grow image border)
            (gimp-edit-fill bkg-layer FILL-BACKGROUND)
            (gimp-selection-layer-alpha bkg-layer)
            
            ;;;;create the outline
            (gimp-context-set-foreground outline-color)
            (gimp-edit-fill bkg-layer FILL-FOREGROUND)
            
            ;;;;create the bubble fill
            (if (= bubble-fill TRUE)
                (begin
                    (gimp-selection-shrink image outline)
                    (gimp-context-set-background bubble-color)
                    (gimp-edit-fill bkg-layer FILL-BACKGROUND)
                )
            )
            (gimp-selection-none image)
        )
    )
    
    (if (= bubble-fill FALSE)
        (begin
            (gimp-selection-layer-alpha bkg-layer)
            (gimp-selection-shrink image outline)
            (if (= outline 0)
                (gimp-selection-shrink image 1)
            )
            (gimp-edit-clear bkg-layer)
            (gimp-selection-none image)
        )
    )
    
    ;;;;add the drop shadow	
    (if (= shadow TRUE)
        (begin
            (script-fu-drop-shadow  image bkg-layer 8 8 15 '(0 0 0) 80 FALSE)
        )
    )
    
    ;;;;finish the script;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
    (if (and(= shadow TRUE) (= conserve FALSE))
        (set! bkg-layer (car (gimp-image-merge-down image bkg-layer CLIP-TO-IMAGE )))
    )
    
    (gimp-drawable-set-name bkg-layer "Comix Bubble")
    (if (= conserve FALSE)
        (begin 
            (set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
            (gimp-drawable-set-name text-layer "Comix Text Bubble")
        )
    ) 
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    
 )
)

(script-fu-register "script-fu-comix-text-bubble"       
    "Comix Text Bubble"
    "Creates a Text Bubble layer top-left of Image \n file:Comix Text Bubble.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Oct 2012"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-TEXT       "Text"    "Comix Text Bubble"
    SF-COLOR      "Text color"         '(0 0 0)
    SF-FONT       "Font"               "Sans Bold"
    SF-ADJUSTMENT "Font size (pixels)" '(50 10 500 1 1 0 1)
    SF-ADJUSTMENT "Additional Width (pixels)" '(0 0 1000 1 10 0 1)
    SF-ADJUSTMENT "Additional Height(pixels)" '(0 0 1000 1 10 0 1)
    SF-OPTION     "Bubble Shape" '("Ellipse" "Rectangle" "Rounded Rectangle" "Shape Hugging")
    SF-OPTION     "Bubble Pointer Type" '("Speech" "Thought" "None")
    SF-OPTION     "Pointer Orientation" '("Lower-Left" "Lower-Right" "Lower-Centre" "Upper-Left" "Upper-Right" "Upper-Centre")
    SF-ADJUSTMENT "Outline Size(pixels)" '(2 0 6 1 1 0 1)
    SF-COLOR      "Outline Color"         '(0 0 0)
    SF-TOGGLE     "Color the Bubble"   TRUE
    SF-COLOR      "Bubble Color"         '(255 255 255)
    SF-TOGGLE     "Add a Drop Shadow"   FALSE
    SF-TOGGLE     "Keep the Layers"   FALSE
)

(script-fu-menu-register "script-fu-comix-text-bubble" "<Image>/Script-Fu/Decor")

(script-fu-menu-register "script-fu-comix-text-bubble" "<Image>/Script-Fu3/Comics-o-matic")

;end of script

; end of script